{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, OverloadedStrings
, TypeApplications, ScopedTypeVariables, OverloadedLabels, ExistentialQuantification
, AllowAmbiguousTypes, FlexibleInstances, DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, NamedFieldPuns
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
                       , chopaanDB
                       , wp
                       , qp
                       , createDB
                       , QueryGenParams(..)
                       , Agg(..)
                       , defaultGenParams
                       ) where

import Prelude hiding ((.))
import GHC.Generics
import Control.Category
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Catch

import Network.HTTP.Client

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
import qualified Database.InfluxDB.Manage as DB
import qualified Database.InfluxDB.JSON as DJ
import qualified Database.InfluxDB.Write.UDP as UDP

import qualified Database.InfluxDB.Write as Http

import qualified Streamly.Internal.Data.Sink as Sink

import qualified Streamly.Internal.Data.Fold as FL

import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Vector as V
import Data.Vector (Vector)
import Chopaan.Utils.Retry
import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.NodeId
import Chopaan.Node.Metrics hiding (Timestamp)
import Chopaan.Node.Storage.Battery
import Chopaan.Node.Folds
import Chopaan.Node.Mesh
import Debug.Trace
import System.IO.Unsafe

data Grouping = GroupTime | GroupTag Key deriving (Eq)

data Agg = Mean | Count deriving (Eq, Show, Generic)


chopaanDB :: Database
chopaanDB = F.formatDatabase "chopaan"

createDB :: Database -> IO ()
createDB d = DB.manage (qp d) $ F.formatQuery ("CREATE DATABASE "F.%F.database) d

deleteDB :: Database -> IO ()
deleteDB d = DB.manage (qp d) $ F.formatQuery ("DELETE DATABASE "F.%F.database) d


wp :: Database -> Http.WriteParams
wp = Http.writeParams

qp :: Database -> QueryParams
qp = queryParams

lineFoldUdp :: forall m. (MonadIO m, MonadMask m) => Int -> UDP.WriteParams -> FL.Fold m [Line UTCTime] ()
lineFoldUdp batchSize wp = FL.many (FL.take batchSize FL.mconcat) lineFold'
  where
    lineFold' = Sink.toFold $ Sink.drainM (recoverC "lineFold" 10 . liftIO . UDP.writeBatch wp)

lineFoldHttp :: forall m. (MonadIO m, MonadMask m) => Int -> Http.WriteParams -> FL.Fold m [Line UTCTime] ()
lineFoldHttp batchSize wp = FL.many (FL.take batchSize FL.mconcat) lineFold'
  where
    lineFold' = Sink.toFold $ Sink.drainM (recoverC "lineFold" 10 . liftIO . Http.writeBatch wp)

renderQuery :: Query -> Text
renderQuery (Query q) = q

data QueryGenParams = QueryGenParams
  { qDB :: Database
  , qRetention :: T.Text
  , qAgg :: Maybe (Agg)
  , templateOnly :: Maybe (Measurement -> Database -> Map Key Key -> Key -> Query)
  } deriving (Generic)

defaultGenParams :: Database -> QueryGenParams
defaultGenParams db = QueryGenParams db "\"autogen\"" (Just Mean) Nothing

wrapAgg :: Agg -> T.Text -> T.Text
wrapAgg Mean m = "mean(" <> m <> ")"
wrapAgg Count m = "count(" <> m <> ")"




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
  seriesQueries :: QueryGenParams -> tag -> [Query]
  default seriesQueries :: QueryGenParams -> tag -> [Query]
  seriesQueries qgp tag = case (templateOnly) of
    Just f ->  (f (measurementName @tag @a) qDB (getTags tag)) <$> (getInfluxKeys @a)
    Nothing -> seriesQuery <$> (getInfluxKeys @a)
    where
      QueryGenParams{qDB, qRetention, qAgg, templateOnly} = qgp
      seriesQuery f = F.formatQuery ( "SELECT "
                                       . F.text
                                       . " FROM "
                                       . F.database
                                       . "."
                                       . F.text
                                       . "."
                                       . F.measurement
                                       . F.text
                                     ) (aggF f) qDB qRetention (measurementName @tag @a) whereC
        where
          unKey (Key a) = a
          aggF :: Key -> Text
          aggF p = case qAgg of
            Nothing -> t
            Just aff -> wrapAgg aff t
            where
              t = (unKey $ F.formatKey ("" . F.key) p)
          whereC :: Text
          whereC = " WHERE ("
            <> (T.intercalate " AND " (eqOn <$> (M.toList $ getTags tag)))
            <> ")"
            where
              eqOn :: (Key, Key) -> Text
              eqOn (t, v) = unKey $ F.formatKey (F.key
                                  . " = "
                                  . F.text) t (singleQuote . unKey $ v)
                where
                  singleQuote x = "\'" <> x <> "\'"
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
  } deriving (Generic, Show)


instance QueryResults Watts where
  parseMeasurement p s t f a = (fmap toWatts) $ do
    let deb = ((show (p, s, t, f, a)))
    trace deb (A.parseJSON $ V.head a)
    
instance QueryResults WattSeconds where
  parseMeasurement p s t f a = (fmap toWattSeconds) $ do
    let deb = ((show (p, s, t, f, a)))
    trace deb (A.parseJSON $ V.head a)

    
nodeQueries :: QueryGenParams -> KbtzNode -> NodeQueries
nodeQueries g kn = NodeQueries (these @PowerNR) (these @EnergyNR) (these @BatteryR) (these @(MeshNode, RxSignal))
  where
    these :: forall a. (HasInfluxFields a, ToMeasurement KbtzNode a) => [Query]
    these = seriesQueries @KbtzNode @a g kn


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
