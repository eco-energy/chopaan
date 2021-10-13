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
                       , chopaanDB
                       , wp
                       , qp
                       , mkNodeQs
                       , chkNodeQs
                       , createDB
                       ) where

import Prelude hiding ((.))
import Control.Category
import Control.Lens
import Control.Monad.IO.Class

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
import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.NodeId
import Chopaan.Node.Metrics hiding (Timestamp)
import Chopaan.Node.Folds
import Chopaan.Node.Mesh
import Debug.Trace
import System.IO.Unsafe

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

chopaanDB :: Database
chopaanDB = F.formatDatabase "chopaan"

createDB :: IO ()
createDB = DB.manage qp $ F.formatQuery ("CREATE DATABASE "F.%F.database) chopaanDB

wp :: Http.WriteParams
wp = (Http.writeParams chopaanDB)
     --{
     --}

qp :: QueryParams
qp = queryParams chopaanDB

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
      seriesQuery f = F.formatQuery ( "SELECT "
                                       . F.key
                                       . " FROM "
                                       . F.database
                                       . "."
                                       . F.text
                                       . "."
                                       . F.measurement
                                       . F.text
                                     ) f chopaanDB "autogen" (measurementName @tag @a) whereC
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

-- pqr :: forall tag a. (IsTag tag, HasInfluxFields a)
--     => Precision 'QueryRequest
--     -> Maybe Text
--     -> HM.HashMap Text Text
--     -> Vector Text
--     -> A.Array
--     -> A.Parser a
-- pqr prec _name _tags columns fields = do
--   let ks = getInfluxKeys @a
--       a = gf <$> ks
--       --getField
--   return a
--   where
--     gf :: forall x. (A.FromJSON x) => Key -> A.Parser x
--     gf (Key f) = DJ.getField f columns fields >>= A.parseJSON 

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


instance QueryResults Watts where
  parseMeasurement p s t f a = (fmap toWatts) $ do
    let deb = ((show (p, s, t, f, a)))
    trace deb (A.parseJSON $ V.head a)
    
instance QueryResults WattSeconds where
  parseMeasurement p s t f a = (fmap toWattSeconds) $ do
    let deb = ((show (p, s, t, f, a)))
    trace deb (A.parseJSON $ V.head a)

--instance (QueryResults e, QueryResults p) => QueryResults (Battery e p)

chkNodeQs :: forall m. (MonadIO m) => NodeQueries -> m ()
chkNodeQs nq = do
  mapM_ mkQ (powerQ nq)
  mapM_ mkQ (energyQ nq)
  mapM_ mkQ (batteryQ nq)
  mapM_ mkQ (meshQ nq)
  where
    pwinte req resp = do
      --print req
      --print (responseHeaders resp)
      print =<< (brConsume $ responseBody resp)
    mkQ q = liftIO $ withQueryResponse qp Nothing q pwinte

mkNodeQs :: forall m. (MonadIO m) => QueryParams -> NodeQueries -> m (Vector (PowerNR), Vector (EnergyNR))
mkNodeQs qp nq = (,) <$> pows <*> es
  where
    toNode [a, b, c] = Node a b c
    --toBattery = undefined
    pows :: m (Vector (Node Watts))
    pows = (fmap sequence) $ (fmap toNode) $ mapM (q @Watts) (powerQ nq)
    es :: m (Vector (Node WattSeconds))
    es = (fmap sequence) $ (fmap toNode) $ mapM (q @WattSeconds) (energyQ nq)
    --ms :: m (Vector (MeshNode, RxSignal))
    --ms = undefined
    --bs :: m (Vector (Battery WattSeconds Watts))
    --bs = (fmap sequence) $ (fmap toBattery) $ mapM (q @WattSeconds) (energyQ nq)
    q :: forall x. (QueryResults x) => Query -> m (Vector x)
    q = liftIO . (query qp)
    
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
