{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeApplications, ScopedTypeVariables, ExplicitForAll, NamedFieldPuns, TupleSections, BangPatterns, PolyKinds, DataKinds, UnboxedTuples #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable, DerivingVia, CPP, LambdaCase, RecordWildCards, RankNTypes, ConstraintKinds, GADTs, TypeSynonymInstances, FlexibleInstances, QuantifiedConstraints, MultiParamTypeClasses #-}
module Chopaan.Hydrate
  ( runHydration
  , Command(..)
  , onCommand
  , unfoldNodes
  , prefixGen
  , LifeTime(..)
  , HydrationConf(..)
  , parseHConf'
  , delayInSeconds
  , toMilli
  ) where

import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId
import Chopaan.Comm.S3 hiding (pathFile)
import Chopaan.Types
import Streamly.Binary
import Chopaan.Kibbutz.AWS.Common hiding (preResolvingManager)
import Chopaan.Utils.Retry (recoverC, recoverOrNothing, recoverWith)
import Chopaan.Comm.Dispatch (accessEnergyState, accessRTS)
import Chopaan.Node.Folds (sensorFold, meshFold, SensorR, MeshR)
import Chopaan.Utils.Time (utcTimeNow)
import Data.Influxable (KbtzNode, asKbtzNode, lineSensorR
                       , lineMesh, lineFoldHttp, showText, chopaanDB, wp)


import Proto.NodeMessageSchema.NodeMessages (MeshFrame, EnergyState, RuntimeStats)
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N

import GHC.Generics hiding (Prefix)

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Lens
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Data.Bifunctor
import Data.Word
import qualified Data.Map.Strict as M
import Data.Int
import Data.Maybe
import Data.Either

import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Data.Time as Time
import Data.Time.Clock.POSIX.Compat (posixSecondsToUTCTime)

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import Network.AWS hiding (Metadata)
import qualified Network.AWS.S3 as S3

import Network.DNS.Resolver
import qualified Network.DNS.Cache as NC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types as NC
import qualified Network.HTTP.Client as NC
import qualified Network.HTTP.Client.Internal as NC (hostAddress)

import Database.InfluxDB.Line (Line)
import qualified Database.InfluxDB.Query as Q
import qualified Database.InfluxDB.Write.UDP as UDP
import qualified Database.InfluxDB.Write as Http
import qualified Database.InfluxDB.Format as F
import qualified Database.InfluxDB.Manage as DB
import qualified Database.InfluxDB.Types as DB

import qualified Codec.Winery as W
import Data.ProtoLens

import Data.Void
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.Data.Stream.IsStream.Transform as S
import qualified Streamly.Internal.Data.Stream.IsStream.Common as S
import qualified Streamly.Internal.Data.Stream.IsStream.Generate as S
import qualified Streamly.Internal.Data.Stream.IsStream.Expand as S
import qualified Streamly.External.ByteString as SBS
import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Array.Foreign.Type as A
import qualified Streamly.Internal.Data.Array.Stream.Foreign as AS
import qualified Streamly.Internal.Data.Time.Units as ST
import Streamly.Internal.Data.IORef.Prim (Prim(..))
--import Dhall hiding (newManager, void)
import System.Directory
import System.Envy



labNodes1 :: [NodeMAC]
labNodes1 = NodeId <$>
  [ "ac:67:b2:11:f3:10"
  , "ac:67:b2:12:07:b0"
  , "7c:9e:bd:47:61:bc"
  , "7c:9e:bd:47:b7:e8"
  , "7c:9e:bd:48:4e:e0"
  , "7c:9e:bd:48:a2:c4"
  , "ac:67:b2:11:e6:e4"
  ]

labNodes :: [NodeMAC]
labNodes = NodeId <$>
  [ "ac:67:b2:11:f3:20",
    "ac:67:b2:1d:e7:f4",
    "8c:aa:b5:97:69:48",
    "8c:aa:b5:95:97:c8",
    "8c:aa:b5:95:8f:9c",
    "ac:67:b2:1c:ec:d8",
    "7c:9e:bd:f5:ec:74",
    "ac:67:b2:11:f0:28"
  ]

type HConM m = (S.MonadAsync m, MonadCatch m, MonadThrow m, MonadMask m)

type HConS t m = (S.IsStream t, HConM m) 

type HConfM m a = ReaderT KbtzConf m a

data LifeTime = Finite | Infinite deriving (Eq, Ord, Show, Generic, Read)

instance Var LifeTime where
  toVar = show
  fromVar = readMaybe

data HydrationConf = HydrationConf
  { storePath :: FilePath
  , s3Bucket :: T.Text
  , startDate :: Date
  , pastRes :: Resolution
  , futureRes :: Resolution
  , lifetime :: LifeTime
  } deriving (Generic, FromEnv)


parseHConf :: IO (HydrationConf)
parseHConf = decodeWithDefaults hConfDef

parseHConf' :: IO (Either String HydrationConf)
parseHConf' = decodeEnv 

hConfDef :: HydrationConf
hConfDef = HydrationConf basePath bucket defDate Day Minute Infinite 
  where
    defDate = Date 1 1 2021
    basePath = "./data/hydration"
    bucket = "dosti-datastream"
  
data KbtzConf = KbtzConf
  { env :: Env
  , bucket :: S3.BucketName
  , kbtzStore :: KbtzStore
  , kbtzName :: KbtzName
  , manager :: NC.Manager
  , startTime :: Time.UTCTime
  , life :: LifeTime
  , writeParams :: Http.WriteParams
  , res :: (Resolution, Resolution)
  } deriving (Generic)


type TNodes = TVar (Set NodeMAC)

type TMap k v = TVar (M.Map k (TVar (Set v))) 

type TKbtzim = TMap KbtzName NodeMAC

lookupTSet :: (Ord k) => k -> TMap k v -> STM (Maybe (Set v))
lookupTSet k tv = do
  m <- readTVar tv
  case M.lookup k m of
    Nothing -> return Nothing
    Just s' -> Just <$> readTVar s'

readKbtzim :: TKbtzim -> STM (M.Map KbtzName (Set NodeMAC))
readKbtzim k = do
    v <- readTVar k
    let xx = M.toList v
    v' <- mapM (\(k', v') -> do
                   v'' <- readTVar v'
                   return (k', v'')
               ) xx
    return $ M.fromList v'

data Control = Control
  { command :: TQueue (Command)
  , kbtzNodes :: TKbtzim
  }

defK :: MonadIO m => m (TKbtzim) 
defK = liftIO . atomically $ do
    ns0 <- newTVar (Set.fromList labNodes)
    ns1 <- newTVar (Set.fromList labNodes1)
    newTVar (M.fromList [(kbtz0, ns0), (kbtz1, ns1)])
    where
      kbtz0 = KbtzId "TestGrid0"
      kbtz1 = KbtzId "TestGrid1"

--drainPar = S.drain . S.fromParallel

mkKbtzConf :: HydrationConf
  -> Env
  -> KbtzName
  -> KbtzStore
  -> NC.Manager
  -> Http.WriteParams
  -> KbtzConf
mkKbtzConf (HydrationConf{s3Bucket
                         , startDate, lifetime
                         , pastRes, futureRes}) env name store manager wp =
  KbtzConf env (S3.BucketName s3Bucket) store name manager (toUTC startDate) lifetime wp (pastRes, futureRes)

runHydration :: HydrationConf -> IO ()
runHydration conf = do
  kbtzim <- defK
  tNow <- Time.getCurrentTime
  aws <- getAwsEnv S3.s3
  man <- newManager
  let p = DB.queryParams chopaanDB
  DB.manage p $ F.formatQuery ("CREATE DATABASE "F.%F.database) chopaanDB
  let
    configureH (kId, kNodes) = do
      let
        store = initKbtzStore kId (storePath conf)
        c = mkKbtzConf conf aws kId store man wp 
      hydrateKbtz c kNodes (unfoldNodes (lifetime conf) kbtzim)
  S.drain . S.fromWAsync $
    S.mapM configureH $ S.unfold (unfoldKbtzim (lifetime conf) kbtzim) ()
  --S.drain $ h --`S.wAsync` control

data Command = StartKbtz KbtzName [NodeMAC]
             | StopKbtz KbtzName
             | StartNode KbtzName NodeMAC
             | StopNode KbtzName NodeMAC
             | ShowState
             deriving (Eq, Ord, Show, Generic)


parseCmd :: (HConS t m) => t m Command
parseCmd = S.delayPre 1 $ S.repeat ShowState

onCommand :: TKbtzim -> Command -> STM ()
onCommand tv (StartKbtz k ns) = do
  m <- readTVar tv
  let s = M.lookup k m
  case s of
    Nothing -> do
      v <- newTVar (Set.fromList ns)
      modifyTVar' tv (M.insert k v)
    Just s' -> modifyTVar' s' (\s'' -> s'' <> (Set.fromList ns))
onCommand tv (StopKbtz k) = modifyTVar' tv (M.delete k)
onCommand tv (StartNode k n) = do
  m <- readTVar tv
  let s = M.lookup k m
  case s of
    Nothing -> return ()
    (Just s') -> modifyTVar s' ((Set.insert n))
onCommand tv (StopNode k n) = do
  m <- readTVar tv
  let s = M.lookup k m
  case s of
    Nothing -> return ()
    (Just s') -> modifyTVar s' (Set.delete n)
onCommand tv ShowState = void $ readKbtzim tv
--getKbtzim :: UF.Unfold m TKbtzim KbtzName
--getKbtzim = UF.unfoldrM ()


unfoldNodes :: forall m. (HConM m) => LifeTime -> TKbtzim -> UF.Unfold m KbtzName NodeMAC
unfoldNodes lt tv = traceUF (liftIO . print) $ UF.many (UF.mkUnfoldM step inject) UF.fromList
  where
    onNullDiff s = case lt of
      Finite -> UF.Stop
      Infinite -> UF.Skip s
    step :: (KbtzName, Set NodeMAC) -> m (UF.Step (KbtzName, Set NodeMAC) [NodeMAC])
    step (k, oldSet) = liftIO . atomically $ do
      newSet <- lookupTSet k tv
      case newSet of
        Nothing -> return $ UF.Stop
        Just s -> do
          let diff = Set.difference s oldSet
          case (null diff) of
            True -> return $ UF.Stop -- UF.Skip (k, s)
            False -> return $ UF.Yield (Set.toList diff) (k, s)
    inject :: KbtzName -> m (KbtzName, Set NodeMAC)
    inject k = return (k, mempty)

getKeys :: (Ord k) => TMap k v -> STM (Set k)
getKeys = (fmap (Set.fromList . M.keys)) . readTVar
    
unfoldKbtzim :: forall m. (HConM m) => LifeTime -> TKbtzim -> UF.Unfold m () (KbtzName, TNodes)
unfoldKbtzim lt tv = traceUF (liftIO . print . fst) $
                  UF.many (UF.mkUnfoldM step inject) UF.fromList
  where
    onNullDiff s = case lt of
      Finite -> UF.Stop
      Infinite -> UF.Skip s
    step :: (Set KbtzName) -> m (UF.Step (Set KbtzName) [(KbtzName, TNodes)])
    step oldSet = liftIO . atomically $ do
      newSet <- getKeys tv
      let diff = Set.difference newSet oldSet
      case (null diff) of
        True -> return $ onNullDiff newSet
        False -> do
          let z = (Set.toList diff)
          ps <- traverse (flip nodeSet' tv) z
          return $ UF.Yield (zip z ps) newSet
    inject :: () -> m (Set KbtzName)
    inject _ = return mempty

nodeSet' :: KbtzName -> TKbtzim -> STM TNodes
nodeSet' kId ks = do
    nodeSet'' <- (M.lookup kId) <$> readTVar ks
    case nodeSet'' of
      Nothing -> retry
      Just s -> return s

nodeSet :: MonadIO m => KbtzName -> TKbtzim -> m TNodes
nodeSet kId = liftIO . atomically . nodeSet' kId


ufStream :: forall m a b. (HConM m) => (a -> S.SerialT m b) -> UF.Unfold m a b
ufStream f = UF.many (UF.function f) UF.fromStream

nodePrefixes :: forall m. (HConM m)
  => KbtzName
  -> (NodeMAC -> IO ())
  -> UF.Unfold m KbtzName NodeMAC
  -> UF.Unfold m NodeMAC Prefix
  -> S.WAsyncT m (NodeMAC, Prefix) 
nodePrefixes kbtzId mkDirs ns ps = do
  n <- S.trace (liftIO . mkDirs) $ S.unfold ns kbtzId
  p <- S.unfold ps n
  return (n, p)


hydrateKbtz :: forall m. (HConM m)
  => KbtzConf
  -> TNodes
  -> UF.Unfold m KbtzName NodeMAC
  -> m ()
hydrateKbtz KbtzConf{kbtzName, kbtzStore, manager, res, startTime, bucket, env, writeParams, life} tNodes ns = do
  t0 <- liftIO $ Time.getCurrentTime
  let
    prefixes' = ufStream (prefixGen life inSet res startTime t0)
    nps = S.adapt $ nodePrefixes kbtzName (mkNodeDirs kbtzStore) ns prefixes'
  void $ S.fold (inFrame kbtzStore writeParams)
    $ S.fromAhead
    $ S.map (second fst)
    S.|$ S.filter ((> 0) . snd . snd)
    S.|$ S.trace (pr . frameLog)
    S.|$ dlFramesParFS manager (bucket, env, t0) (getKbtzPath kbtzStore)
    S.|$ S.fromWAsync $ S.maxThreads 3500
    S.|$ S.map fst
    S.|$ S.filter ((> 0) . snd)
    S.|$ getKeysUF env bucket nps (fileSaver Keys kbtzStore)
  where
    inSet n = liftIO @m . atomically $ do
      s <- readTVar tNodes
      return $ Set.member n s
    {-# INLINE inSet #-}
    pr = liftIO . print
    prefLog (n, p) = "Prefix Generated :" <> (show (n, p))
    keyLog (n, p) = "Keys Downloaded for Node :" <> (show n) <> " and Prefix :" <> (show p)
    frameLog (n, p) = "Frames Downloaded for Node :" <> (show n) <> " and Prefix :" <> (show p)

prefixGen :: forall m a. (S.MonadAsync m, Ord a)
  => LifeTime
  -> (a -> m Bool)
  -> (Resolution, Resolution)
  -> Time.UTCTime
  -> Time.UTCTime
  -> a
  -> S.SerialT m Prefix 
prefixGen life keep (pastRes, futureRes) start now n = case life of
  Finite -> past
  Infinite -> S.uniq (past <> future)
  where
    past = S.fromList p'
    {-# INLINE past #-}
    future = S.takeWhileM (\_ -> (keep n)) $ S.delayPre (delayInSeconds diff futureRes)
      (S.unfold UF.enumerateFromStepIntegral (lastP, nextP))
    {-# INLINE future #-}
    diff = nextP - lastP
    lp = last p'
    p' = prefixRange pastRes start now
    lastP = toMilli (*) lp pastRes
    nextP = toMilli (+) lastP futureRes
{-# INLINE prefixGen #-}

delayInSeconds :: Prefix -> Resolution -> Double
delayInSeconds diff futureRes = (((resDiff futureRes) *) . realToFrac . unPrefix $ diff)

toMilli :: (Prefix -> Prefix -> Prefix) -> Prefix -> Resolution -> Prefix
toMilli op a r = a `op` (10 ^ (l10 . resDiff $ r))
  where
    l10 = ceiling @Double @Prefix . logBase 10

delF :: Resolution -> Int64
delF = ceiling . (10 **) . realToFrac . (ceiling . logBase 10 . resDiff)
{-# INLINE delF #-}

deriving newtype instance W.Serialise ST.MilliSecond64

newtype S3Id = S3Id ST.MilliSecond64
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Bounded, Enum, Integral, Num, Real, W.Serialise)
  deriving newtype (Prim)

deriving newtype instance W.Serialise S3.ObjectKey

toS3Id :: S3.ObjectKey -> S3Id
toS3Id = S3Id . ST.MilliSecond64 . read . T.unpack . snd . (T.breakOnEnd ("/")) . (T.replace " " "") . unObject

newtype S3Idx a = S3Idx (S3Id, a)
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving (W.Serialise) via (W.WineryProduct (S3Idx a))

toS3Idx :: (S3Id, a) -> S3Idx a
toS3Idx = S3Idx

type S3Key = S3Idx S3.ObjectKey

type S3Req = S3Idx BS.ByteString

type S3Body = (S3Idx (BS.ByteString))

type GetObjError = (S3Idx RespStatus)

newtype RespStatus = RespStatus (Int, BS.ByteString)
  deriving (Eq, Ord, Show, Generic)
  deriving W.Serialise via (W.WineryProduct RespStatus)

wrapStatus :: NC.Status -> RespStatus
wrapStatus s = RespStatus (NC.statusCode s, NC.statusMessage s)

type S3Resp = Either GetObjError S3Body

(</>) :: FilePath -> FilePath -> FilePath 
a </> b = a <> "/" <> b

data StoreType = Keys | Frames | Errors | Ingested
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

storeTypeName :: StoreType -> FilePath
storeTypeName = T.unpack . T.toLower . showText

data KbtzStore = KbtzStore
  { mkNodeDirs :: NodeMAC -> IO ()
  , keyFolder :: NodeMAC -> FilePath
  , frameFolder :: NodeMAC -> FilePath
  , errFolder :: NodeMAC -> FilePath
  , ingestedFolder :: NodeMAC -> FilePath
  , keyFile :: (NodeMAC -> Prefix -> FilePath)
  , frameFile :: (NodeMAC -> Prefix -> FilePath)
  , errFile :: (NodeMAC -> Prefix -> FilePath)
  , ingestedFile :: (NodeMAC -> Prefix -> FilePath)
  , tagger :: (NodeMAC -> KbtzNode)
  } deriving (Generic)



getKbtzFolder :: KbtzStore -> StoreType -> (NodeMAC -> FilePath)
getKbtzFolder KbtzStore{..} = \case
  Keys -> keyFolder
  Frames -> frameFolder
  Errors -> errFolder
  Ingested -> ingestedFolder

getKbtzPath :: KbtzStore -> StoreType -> (NodeMAC -> Prefix -> FilePath)
getKbtzPath KbtzStore{..} = \case
  Keys -> keyFile
  Frames -> frameFile
  Errors -> errFile
  Ingested -> ingestedFile


traceUF :: (Monad m) => (a -> m b) -> UF.Unfold m x a -> UF.Unfold m x a
traceUF f = UF.mapM (\a -> f a >> (pure a))

initKbtzStore :: KbtzName -> FilePath -> KbtzStore
initKbtzStore kbtz base = KbtzStore mkNodeDirs
    (hFolder Keys) (hFolder Frames) (hFolder Errors) (hFolder Ingested)
    (hFile Keys) (hFile Frames) (hFile Errors) (hFile Ingested) (asKbtzNode kbtz)
  where
    root = base </> (T.unpack . showText $ kbtz)
    nodeDirHere here n =  root </> here </> (nodeMACPath n)
    mkNodeDirHere p n = cd (nodeDirHere p n)
    mkNodeDirs :: NodeMAC -> IO ()
    mkNodeDirs n = mapM_ (\p -> mkNodeDirHere p n) storeDirNames
      where
        storeDirNames :: [FilePath]
        storeDirNames = storeTypeName <$>  [(minBound @StoreType)..maxBound]
    hFolder :: StoreType -> NodeMAC -> FilePath
    hFolder s n = base
                  </> (T.unpack (unKbtzId kbtz))
                  </> (storeTypeName s)
                  </> (nodeMACPath n)
    hFile :: StoreType -> NodeMAC -> Prefix -> FilePath
    hFile s n pref = hFolder s n
                     </> (T.unpack . asFileName $ pref)


-- ingestKbtzFrames :: HConM m
--   => KbtzStore
--   -> Http.WriteParams
--   -> m ()
-- ingestKbtzFrames store writeParams = S.drain
--   $ S.fromParallel
--   $ S.mapM (ingestNodeFramesFS store fl) $ S.unfold0 (nodes store)
--   where
--     fl = lineFoldHttp 32 writeParams

nodeDirUF :: forall m. HConM m => KbtzStore -> StoreType -> UF.Unfold m NodeMAC Word8
nodeDirUF store stage = uf
  where
    ps :: UF.Unfold m NodeMAC FilePath
    ps = UF.lmap (getKbtzFolder store stage) Dir.readFiles
    uf :: UF.Unfold m NodeMAC Word8
    uf = UF.many ps File.read


-- ingestNodeFramesFS :: forall m. (HConM m) => KbtzStore -> FL.Fold m [Line Time.UTCTime] () -> NodeMAC -> m ()  
-- ingestNodeFramesFS store lnFold n =
--   ingestFrames (nodeDirUF store Frames) lnFold (tagger store) n


inFrame :: forall m. (HConM m)
  => KbtzStore
  -> Http.WriteParams
  -> FL.Fold m (NodeMAC, Prefix) (M.Map NodeMAC ()) 
inFrame store writeParams = FL.classifyWith (fst) ingestNode 
  where
    ingestNode :: FL.Fold m (NodeMAC, Prefix) ()
    ingestNode = (FL.mkFoldM iN (pure $ FL.Partial ((), Nothing)) (pure . fst))
      where
        iN (_, oldF) (n, p) = do
          let thisF = fromMaybe (nodeFold n) oldF
          (r, newF) <- ingestFrames (readNP p) thisF (FL.lmap nodeLines $ fl)  n
          return $ FL.Partial (r, newF)
          where
            tag = (tagger store $ n)
            nodeLines = (\(a, b) -> a <> b) . bimap (lineSensorR tag) (lineMesh tag)
            nodeFold n' = (FL.partition sensorFold (meshFold n'))
    readNP p = UF.many (UF.function (\n' -> getKbtzPath store Frames n' p)) File.read
    fl = lineFoldHttp 32 writeParams

type GF m = FL.Fold m (Either EnergyState RuntimeStats) (SensorR, MeshR)


ingestFrames :: forall m r. (S.MonadAsync m, MonadCatch m)
  => UF.Unfold m NodeMAC Word8
  -> GF m --FL.Fold m (Either EnergyState RuntimeStats) (SensorR, MeshR)
  -> FL.Fold m (SensorR, MeshR) r
  -> NodeMAC
  -> m (r, Maybe (GF m))
ingestFrames source nodeFold lnFold n = do
  S.fold (FL.unzip lnFold FL.last)
    . S.fromAhead
    . S.maxThreads 3000
    . S.tapRate 60 (\r -> liftIO $ print $ "Ingest Rate for " <> (show n) <> " : " <> (show r))
    . S.postscan (FL.tee nodeFold (FL.duplicate nodeFold))
    . S.catMaybes
    . S.trace (logNothing)
    . S.mapM (pure . validateMF')
    . S.rights
    . S.trace (logEither)
    . S.mapM (pure . parsePB)
    . decodeFrames
    $ S.unfold source n
  where
    parsePB = (traverse ((fmap fromPB) . decodeA @(PB MeshFrame) . SBS.toArray))
    validateMF' :: S3Idx MeshFrame -> Maybe (Either EnergyState RuntimeStats)
    validateMF' (S3Idx ((S3Id idx), m)) = case accessEnergyState m of
      Just !e -> Just . Left $ fixGridTS t e
      Nothing ->
        case accessRTS m of
          Just !r -> Just . Right $ fixMeshTS t r
          Nothing -> Nothing
     where
       t = Just . posixSecondsToUTCTime . fromIntegral $ div idx 1000 
    decodeFrames :: forall t . (S.IsStream t) => t m Word8 -> t m (S3Body)
    decodeFrames = S.map (fromWino)
      . S.rights
      . S.trace (logEither)
      . decodeS @t @m @(Wino S3Body)


commonPref :: Prefix -> Prefix -> Maybe (T.Text, T.Text, T.Text)
commonPref (Prefix p) (Prefix p') = T.commonPrefixes (T.pack . show $ p) (T.pack . show $ p')

dlFramesParFS ::
  forall t m. HConS t m
  => NC.Manager
  -> SignWith
  -> (StoreType -> NodeMAC -> Prefix -> FilePath)
  -> t m (NodeMAC, Prefix)
  -> t m (NodeMAC, (Prefix, Int))
dlFramesParFS man sw getPath ps = S.concatMapM dlM $ S.trace (liftIO . print) $ S.foldMany congregatePrefixes ps
  where
    commonPrefixF :: A.Array Prefix -> m Prefix
    commonPrefixF a = (fromMaybe 1)
                     <$> (A.fold (FL.foldl' comm Nothing) a)
      where
        comm Nothing p' = Just p'
        comm (Just p) p' = (\(x, _, _) -> (Prefix . read . T.unpack) x)
                           <$> (commonPref p p')
    congregatePrefixes = FL.lmapM (\a -> (liftIO . print $ a) >> (return a)) $
                         (FL.classifyWith fst (FL.takeInterval 10 (FL.lmap snd A.write)))
    dlM = fmap (S.fromList . M.toList) . (M.traverseWithKey dlF)
    dlF :: NodeMAC -> A.Array Prefix -> m (Prefix, Int)
    dlF n s = do
      comPref <- commonPrefixF s
      r <- download man sw (n, comPref) (frameSink comPref) (errSink comPref) keySource
      return $ r
      where
        frameSink p = saveWithLength p (encodeFold (getPath Frames n p))
        errSink p = encodeFold (getPath Errors n p)
        keySource :: forall t1. (S.IsStream t1) => t1 m (S3Key)
        keySource = decodeKeys (S.unfold readAll s)
          where
            readAll = UF.many ((getPath Keys n) <$> A.read) File.read
            decodeKeys :: t1 m Word8 -> t1 m (S3Key)
            decodeKeys = S.map (fromWino)
              . S.rights
              . S.trace (logEither)
              . decodeS @t1 @m @(Wino S3Key)
{-# INLINE dlFramesParFS #-}

saveWithLength :: (MonadIO m) => b -> FL.Fold m a () -> FL.Fold m a (b, Int) 
saveWithLength tag f = FL.rmapM (\x -> (liftIO . print $ x) >> (return (tag, (snd x))))
                       (FL.tee f FL.length)


-- downloadNodeFS :: forall m. HConM m
--   => NC.Manager
--   -> KbtzStore
--   -> SignWith
--   -> UF.Unfold m NodeMAC Prefix
--   -> NodeMAC
--   -> m () 
-- downloadNodeFS man store sw ps n = S.drain
--     $ S.fromWAsync
--     $ S.mapM (\p -> (downloadFS man sw (keySource p) (frameSink p) (errSink p) n))
--     $ S.unfold ps n 
--   where
--     frameSink = (getKbtzPath store Frames n)
--     errSink = (getKbtzPath store Errors n)
--     keySource = (getKbtzPath store Keys n)
-- {-# INLINE downloadNodeFS #-}

-- downloadFS :: forall m a. (HConM m, Show a)
--   => NC.Manager -> SignWith -> FilePath -> FilePath -> FilePath -> a -> m ()
-- downloadFS man sw sourceKey sinkFrame sinkErr tag =
--   download man sw tag (encodeFold sinkFrame) (encodeFold sinkErr) (loadFile sourceKey)
-- {-# INLINE downloadFS #-}

download :: forall m tag r. (HConM m, Show tag)
  => NC.Manager
  -> SignWith
  -> tag
  -> FL.Fold m (Wino S3Body) r
  -> FL.Fold m (Wino GetObjError) ()
  -> (forall t. S.IsStream t => t m S3Key)
  -> m r
download man signWith tag saveDL saveErr = (fmap snd)
  . S.fold (FL.partition saveErr saveDL)
  . S.map (bimap toWino toWino)
  . S.tapRate 60 (printDLRate)
  . S.fromAhead
  . (dl man signWith)
  where
    printDLRate r = liftIO . print
      $ "Download rate from Node "
      <> (show tag)
      <> " is " <> (show r)
{-# INLINE download #-}


dl :: forall m. HConM m => NC.Manager -> SignWith -> S.AheadT m S3Key -> S.AheadT m S3Resp
dl man signWith = S.trace (logEither)
                  . S.maxThreads 1000
                  . S.mapM (goIdxd)
                  . S.mapM (signIdxd)
                  . S.maxRate 5500
  where
    goIdxd :: S3Req -> m (S3Resp)
    goIdxd (S3Idx (idx, bs)) = do
      o <- getObject man bs
      return $ bimap (S3Idx . (idx,)) (S3Idx . (idx,)) o
    signIdxd :: S3Key -> m (S3Req)
    signIdxd = traverse (signGetObject signWith)
{-# INLINE dl #-}

type SignWith = (S3.BucketName, Env, Time.UTCTime)

signGetObject :: (MonadIO m) => SignWith -> S3.ObjectKey -> m (BS.ByteString)
signGetObject (bucket, env, t) = sign
  where
    sign w = liftIO $ withAwsEnv env (liftAWS . presignURL t (oneHour) . toReq $ w)
    toReq k = S3.getObject bucket k
    oneHour = 60 * 60
{-# INLINE signGetObject #-}

getObject :: (MonadIO m) => NC.Manager -> BS.ByteString -> m (Either RespStatus (BS.ByteString))
getObject manager req = liftIO $ do
  r <- NC.parseRequest . T.unpack . T.decodeUtf8 $ req
  let r' = r{ NC.secure = False, NC.port = 80}
  recoverWith ("req" :: String) 3
    (Left . wrapStatus $ NC.imATeapot418)
    (NC.withResponse r' manager checkResponse)
  where
    checkResponse :: NC.Response NC.BodyReader -> IO (Either RespStatus (BS.ByteString)) 
    checkResponse resp = case (NC.responseStatus resp == NC.ok200) of
                           True -> do
                             bo <- BS.concat <$> (NC.brConsume . NC.responseBody $ resp)
                             return . Right $ bo
                           --_ -> threadDelay 10
                           _ -> return . Left . wrapStatus $ NC.responseStatus resp
{-# INLINE getObject #-}

-- getKeysFS :: forall t m. (HConS t m) => t m (NodeMAC, Prefix) -> HConfM m (t m (NodeMAC, Prefix))
-- getKeysFS nps = do
--   KbtzConf{env, bucket, kbtzStore} <- ask
--   getKeysUF env bucket nps (fileSaver Keys kbtzStore)
-- {-# INLINE getKeysFS #-}


fileSaver :: (HConM m) => StoreType -> KbtzStore -> NodeMAC -> Prefix -> FL.Fold m S3Key ((NodeMAC, Prefix), Int)
fileSaver ty store n t = saveWithLength (n, t) (FL.lmap toWino (encodeFold path))
  where
    path = (getKbtzPath store ty n t)


getKeysUF :: forall t m r. (HConS t m, Show r)
  => Env
  -> S3.BucketName
  -> t m (NodeMAC, Prefix)
  -> (NodeMAC -> Prefix -> FL.Fold m S3Key r)
  -> t m r
getKeysUF env bucket ns prefixFold = S.mapM (uncurry prefixKeys) ns
  where
    logPrefGen p = liftIO . print $ "prefix: " <> (show p)
    prefixKeys :: NodeMAC -> Prefix -> m r
    prefixKeys n t = do
      liftIO . print $ ("starting ting" <> (show (n, t)))
      r <- ((UF.fold
              (prefixFold n t)
              (UF.map (toS3Idx . (toS3Id &&& id) . fst) (s3Paths'' env (req n)))) t)
      liftIO . print $ ("finished writing keys" <> (show (n, t, r)))
      --recoverC (show n <> " " <> show t) 3 
      return $ r
      where
        -- FL.lmapM (\a -> (liftIO . print $ a) >> (return a)) $ 
        req n' prefix = S3.listObjectsV2 bucket & S3.lovPrefix .~ (timedPrefix n' prefix)
{-# INLINE getKeysUF #-}


-- loadFile :: forall t m a. (HConS t m, W.Serialise a)
--                 => FilePath -> t m a
-- loadFile path = S.map (fromWino)
--       $ S.rights
--       $ S.trace (logEither)
--       $ decodeFile path
-- {-# INLINE loadFile #-}




newManager :: (MonadIO m) => m NC.Manager
newManager = liftIO $ NC.newManager cachingSettings
  where
    cachingSettings = NC.defaultManagerSettings --tlsManagerSettings
      { --NC.managerConnCount = 1024
      --, NC.managerIdleConnectionCount = 
      NC.managerResponseTimeout = NC.responseTimeoutMicro (90 * oneSec)
      -- , NC.managerModifyRequest = preResolveReq c  
      }
      where
        oneSec = 1000000
    preResolveReq cache r = do
      h <- liftIO $ NC.lookup cache (NC.host r)
      let r' = r { NC.hostAddress = h }
      return r'


logNothing :: forall m a. (MonadIO m, Show a) => Maybe a -> m ()
logNothing = \x -> if (isNothing x) then (liftIO $ print x) else (return ())
{-# INLINE logNothing #-}

logEither :: forall m a b. (MonadIO m, Show a) => Either a b -> m ()
logEither x = case x of
  Left a -> liftIO $ print a
  Right _ -> return ()
{-# INLINE logEither #-}




-- data Metadata = Metadata
--   { mKeys :: !Int
--   , mFrame :: !Int
--   , mError :: !Int
--   --, mLoad :: !Int
--   }
--   deriving (Eq, Ord, Show, Generic)
--   deriving (W.Serialise) via (W.WineryRecord Metadata) 


-- mkMetadata :: forall m. (S.MonadAsync m, MonadCatch m)
--            => KbtzStore -> NodeMAC -> Prefix -> m Metadata
-- mkMetadata s n p = Metadata
--                    <$> (fileLen $ loadPrefixKeys s n p)
--                    <*> (fileLen $ loadPrefixFrames s n p)
--                    <*> (fileLen $ loadPrefixErrors s n p)
--   where
--     fileLen = S.fold FL.length




