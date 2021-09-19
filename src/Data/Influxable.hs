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
                       , showText
                       , lineFoldUdp
                       , lineFoldHttp
                       ) where

import Prelude hiding ((.))
import Control.Category
import Control.Lens
import Control.Monad.IO.Class

import Debug.Trace
import Data.Maybe
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
import qualified Database.InfluxDB.Write.UDP as UDP

import qualified Database.InfluxDB.Write as Http

import qualified Streamly.Internal.Data.Sink as Sink

import qualified Streamly.Internal.Data.Fold as FL

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


lineFoldUdp :: forall m. (MonadIO m) => Int -> UDP.WriteParams -> FL.Fold m [Line UTCTime] ()
lineFoldUdp batchSize wp = FL.many (FL.take batchSize FL.mconcat) lineFold'
  where
    lineFold' = Sink.toFold $ Sink.drainM (liftIO . UDP.writeBatch wp)

lineFoldHttp :: forall m. (MonadIO m) => Int -> Http.WriteParams -> FL.Fold m [Line UTCTime] ()
lineFoldHttp batchSize wp = FL.many (FL.take batchSize FL.mconcat) lineFold'
  where
    lineFold' = Sink.toFold $ Sink.drainM (liftIO . Http.writeBatch wp)

renderQuery :: Query -> Text
renderQuery (Query q) = q

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
  {-# INLINE mkLine #-}
  seriesQueries :: tag -> [Query]
  default seriesQueries :: tag -> [Query]
  seriesQueries tag = seriesQuery <$> (getInfluxKeys @a)
    where
      seriesQuery f = (F.formatQuery ( "SELECT "
                                       . F.key
                                       . " FROM "
                                       . F.measurement
                                       . F.text
                                     ) f (measurementName @tag @a)) whereC
        where
          whereC :: Text
          whereC = " WHERE " <>
            (T.intercalate " AND " (eqOn <$> (M.toList $ getTags tag)))
            where
              eqOn :: (Key, Key) -> Text
              eqOn (t, v) = unKey $ F.formatKey (F.key
                                  . " = "
                                  . F.key) t v
                where
                  unKey (Key a) = a
  {-# INLINE seriesQueries #-}


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


lineSensorR :: KbtzNode -> SensorR -> [Line UTCTime]
lineSensorR tag s = [ lineNow . _powerT $ s
                    , lineNow . _energyT $ s
                    , lineNow . _battery $ s
                    ]
    where
      lineNow :: forall a. (HasInfluxFields a, ToMeasurement KbtzNode a) => a -> Line UTCTime
      lineNow = mkLine (const t) tag
      t = _time s 

lineMesh :: KbtzNode -> (MeshNode, RxSignal) -> [Line UTCTime]
lineMesh tag m = [ lineNow m ]
    where
      lineNow :: forall a. (HasInfluxFields a, ToMeasurement KbtzNode a) => a -> Line UTCTime
      lineNow = mkLine (const (Just t)) tag
      t = nodeTime . fst $ m 

instance ToMeasurement (KbtzNode) (PowerNR)  where
  measurementName = "power"

instance ToMeasurement (KbtzNode) (EnergyNR) where
  measurementName = "energy"

instance ToMeasurement (KbtzNode) (BatteryR) where
  measurementName = "battery"

instance ToMeasurement (KbtzNode) (MeshNode, RxSignal) where
  measurementName = "mesh"

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
  getInfluxFields n = toSafeMap (M.fromList $ [ ("tx", getField $ tx n)
                                              , ("consumed", getField $ consumed n)
                                              , ("generated", getField $ generated n)
                                              ])


instance (FieldType e, FieldType p) => HasInfluxFields (Battery e p) where
  getInfluxKeys = ["soc", "chargeLim", "dischargeLim", "totalCapacity"]
  getInfluxFields n = toSafeMap (M.fromList $ [("soc", getField $ soc n)
                                              , ("chargeLim", getField $ chargeLim n)
                                              , ("dischargeLim", getField $ dischargeLim n)
                                              , ("totalCapacity", getField $ totalCapacity n)
                                              ]) 

instance HasInfluxFields (RxSignal) where
  getInfluxKeys = ["strength", "parent"] 
  getInfluxFields n = toSafeMap (M.fromList $ [ ("strength", getField $ strength n)
                                              , ("parent", getField $ parent n)
                                              ]) 

instance HasInfluxFields (MeshNode) where
  getInfluxKeys = ["isRoot", "uptime", "routerRSSI"]
  getInfluxFields n = toSafeMap
                      (M.fromList $ [ ("isRoot", getField $ isRoot n)
                                    , ("uptime", getField $ uptime n)
                                    , ("routerRSSI", getField $ routerRSSI n)
                                    ])

toSafeMap = M.map (fromJust) . M.filter (isJust) --  . trace (show m) $ m

showText :: (Show a) => a -> Text
showText = T.replace "\"" "" . T.pack . show



class FieldType f where
  getField :: f -> Maybe (Field n)


-- instance FieldType a where
--   getField = const Nothing

instance {-# OVERLAPPING #-} FieldType Int64 where
  getField = Just . fromInt

instance {-# OVERLAPPING #-} FieldType Int where
  getField = Just . fromInt . fromIntegral



instance {-# OVERLAPPING #-} FieldType Double where
  getField = Just . fromDouble

instance {-# OVERLAPPING #-} FieldType Bool where
  getField = Just . fromBool

instance {-# OVERLAPPING #-} FieldType NodeMAC where
  getField = Just . fromText . unNodeId

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
