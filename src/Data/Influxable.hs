{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, OverloadedStrings
, TypeApplications, ScopedTypeVariables, OverloadedLabels, ExistentialQuantification
, AllowAmbiguousTypes, FlexibleInstances
, DataKinds, QuantifiedConstraints, ImpredicativeTypes, DefaultSignatures, FlexibleContexts
#-}
module Data.Influxable (asKbtzNode
                       , lineSensorR
                       , lineMesh
                       , nodeQueries
                       , renderQuery
                       , NodeQueries(..)
                       , KbtzNode
                       , nodeName
                       , kbtzName
                       ) where

import Prelude hiding ((.))
import Control.Category

import Data.Int
import Data.Bifunctor
import Data.HList
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Maybe (fromJust)

import qualified Database.InfluxDB.Format as F
import Database.InfluxDB.Types
import Database.InfluxDB.Line
import Database.InfluxDB.Query

import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.NodeId
import Chopaan.Node.Metrics hiding (Timestamp)
import Chopaan.Node.Folds
import Chopaan.Node.Mesh

data Grouping = GroupTime | GroupTag Key deriving (Eq)

data Agg = Mean | Count deriving (Eq)

-- data WhereOp = EqOp | NEqOp -- | GTOp | GTEOp | LTOp | LTEOp 

-- getOp :: WhereOp -> Text
-- getOp o = case o of
--   EqOp -> " = "
--   NEqOp -> " != "

-- data InfluxQ = Select ![(QueryField, Agg)]
--              | From !Measurement
--              | Where ![(Key, Key, WhereOp)]
--              | GroupBy Grouping


renderQuery :: Query -> Text
renderQuery = T.decodeUtf8 . F.fromQuery

-- $ Measurement Construction depends on the ability to construct
-- $ a Tagset and a Fieldset for a datatype

class (IsTag tag, HasInfluxFields a) => ToMeasurement tag a where
  measurementName :: Measurement
  mkLine :: Timestamp time => (a -> Maybe time) -> tag -> a -> Line time
  default mkLine :: Timestamp time => (a -> Maybe time) -> tag -> a -> Line time
  mkLine getTime tag a = Line
    (measurementName @tag @a)
    (getTags tag)
    (getInfluxFields a)
    (getTime a)
  seriesQueries :: tag -> [Query]
  default seriesQueries :: tag -> [Query]
  seriesQueries tag = seriesQuery <$> (getInfluxKeys @a)
    where
      seriesQuery f = (F.formatQuery ( "SELECT "
                                       . F.key
                                       . " FROM "
                                       . F.measurement
                                       . F.key
                                     ) f (measurementName @tag @a)) whereClause
        where
          whereClause :: Key
          whereClause = foldl (\p (predicate, match) ->
                                 F.formatKey (F.key
                                              . " AND "
                                              . F.key) p (eqOn predicate match))
                        (" WHERE ")
                        (M.toList (getTags tag))
            where
              eqOn :: Key -> Key -> Key
              eqOn t v = F.formatKey (F.key
                                  . " = "
                                  . F.key) t v

type KbtzNode = HList '[KbtzName, NodeMAC]

asKbtzNode :: KbtzName -> NodeMAC -> KbtzNode
asKbtzNode k n = k %: n %: HNil 

nodeName :: KbtzNode -> NodeMAC
nodeName (HCons _ (HCons n HNil)) = n

kbtzName :: KbtzNode -> KbtzName
kbtzName (HCons k (HCons _ HNil)) = k

data NodeQueries = NodeQueries
  { powerQ :: [Query]
  , energyQ :: [Query]
  , batteryQ :: [Query]
  , meshQ :: [Query]
  }


nodeQueries :: KbtzNode -> NodeQueries
nodeQueries kn = NodeQueries (these @PowerNR) (these @EnergyNR) (these @BatteryR) (these @(MeshNode, RxSignal))
  where
    these :: forall a. (HasInfluxFields a, ToMeasurement KbtzNode a) => [Query]
    these = seriesQueries @KbtzNode @a kn


lineSensorR :: KbtzName -> NodeMAC -> SensorR -> [Line UTCTime]
lineSensorR k n s = [ lineNow . _powerT $ s
                    , lineNow . _energyT $ s
                    , lineNow . _battery $ s
                    ]
    where
      lineNow :: forall a. (HasInfluxFields a, ToMeasurement KbtzNode a) => a -> Line UTCTime
      lineNow = mkLine (const t) tag 
      tag = asKbtzNode k n
      t = _time s 

lineMesh :: KbtzName -> NodeMAC -> (MeshNode, RxSignal) -> [Line UTCTime]
lineMesh k n m = [ lineNow m ]
    where
      lineNow :: forall a. (HasInfluxFields a, ToMeasurement KbtzNode a) => a -> Line UTCTime
      lineNow = mkLine (const (Just t)) tag
      tag = asKbtzNode k n
      t = nodeTime . fst $ m 

instance ToMeasurement (KbtzNode) (PowerNR)  where
  measurementName = "powerNode"

instance ToMeasurement (KbtzNode) (EnergyNR) where
  measurementName = "energyNode"

instance ToMeasurement (KbtzNode) (BatteryR) where
  measurementName = "battery"

instance ToMeasurement (KbtzNode) (MeshNode, RxSignal)

-- $ Tags Construction
class IsTag i where
  getTags :: i -> Map Key Key

instance (Show a) => IsTag (KbtzId a) where
  getTags (KbtzId k) = M.singleton "kbtzId" (F.formatKey F.text $ showText k)

instance (Show a) => IsTag (NodeId a) where
  getTags (NodeId k) = M.singleton "nodeId" (F.formatKey F.text $ showText k)


instance (IsTag t0, IsTag t1) => IsTag (HList '[t0, t1]) where
  getTags (HCons x (HCons y HNil)) = M.union (getTags x) (getTags y)



-- $ Fields Construction
class HasInfluxFields a where
  getInfluxKeys :: [Key]
  getInfluxFields :: a -> Map Key (Field n)

instance (HasInfluxFields a, HasInfluxFields b) => HasInfluxFields (a, b) where
  getInfluxKeys = (getInfluxKeys @a) <> (getInfluxKeys @b)
  getInfluxFields (a, b) = (getInfluxFields a) <> (getInfluxFields b) 

instance (FieldType a) => HasInfluxFields (Node a) where
  getInfluxKeys = ["tx", "consumed", "generated"]
  getInfluxFields n = M.fromList $ (second (fromJust . getField))
    <$> [ ("tx", tx n)
        , ("consumed", consumed n)
        , ("generated", generated n)
        ] 


instance (FieldType e, FieldType p) => HasInfluxFields (Battery e p) where
  getInfluxKeys = ["soc", "chargeLim", "dischargeLim", "totalCapacity"]
  getInfluxFields n = M.fromList $ [("soc", getFieldUnsafe $ soc n)
                                   , ("chargeLim", getFieldUnsafe $ chargeLim n)
                                   , ("dischargeLim", getFieldUnsafe $ dischargeLim n)
                                   , ("totalCapacity", getFieldUnsafe $ totalCapacity n)
                                   ] 

instance HasInfluxFields (RxSignal) where
  getInfluxKeys = ["strength", "parent"] 
  getInfluxFields n = M.fromList $ [ ("strength", getFieldUnsafe $ strength n)
                                   , ("parent", getFieldUnsafe $ parent n)
                                   ] 

instance HasInfluxFields (MeshNode) where
  getInfluxKeys = ["isRoot", "uptime", "routerRSSI"]
  getInfluxFields n = M.fromList $ [ ("isRoot", getFieldUnsafe $ isRoot n)
                                   , ("uptime", getFieldUnsafe $ uptime n)
                                   , ("routerRSSI", getFieldUnsafe $ routerRSSI n)
                                   ] 


showText :: (Show a) => a -> Text
showText = T.replace "\"" "" . T.pack . show


getFieldUnsafe :: (FieldType a) => a -> Field n
getFieldUnsafe = fromJust . getField


class FieldType f where
  getField :: f -> Maybe (Field n)


instance FieldType a where
  getField = const Nothing

instance {-# OVERLAPPING #-} FieldType Int64 where
  getField = Just . fromInt

instance {-# OVERLAPPING #-} FieldType Double where
  getField = Just . fromDouble

instance {-# OVERLAPPING #-} FieldType Bool where
  getField = Just . fromBool

instance {-# OVERLAPPING #-} FieldType Text where
  getField = Just . fromText

instance {-# OVERLAPPING #-} FieldType Watts where
  getField = getField . fromWatts

instance {-# OVERLAPPING #-} FieldType WattSeconds where
  getField = getField . fromWattSeconds

instance {-# OVERLAPPING #-} (FieldType a) => FieldType (Maybe a) where
  getField a = getField =<< a

fromInt :: Int64 -> Field n
fromInt = FieldInt

fromDouble :: Double -> Field n
fromDouble = FieldFloat

fromBool :: Bool -> Field n
fromBool = FieldBool

fromText :: Text -> Field n
fromText = FieldString


-- class IsFieldType a where
--   isFieldType :: Bool

-- instance IsFieldType a where
--   isFieldType = False

-- instance {-# OVERLAPPING #-} IsFieldType Int64 where
--   isFieldType = True

-- instance {-# OVERLAPPING #-} IsFieldType Double where
--   isFieldType = True

-- instance {-# OVERLAPPING #-} IsFieldType Bool where
--   isFieldType = True

-- instance {-# OVERLAPPING #-} IsFieldType Text where
--   isFieldType = True
