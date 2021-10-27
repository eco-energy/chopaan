{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeApplications, ScopedTypeVariables, ExplicitForAll, NamedFieldPuns, TupleSections, BangPatterns, PolyKinds, DataKinds, UnboxedTuples #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable, DerivingVia, CPP, LambdaCase, RecordWildCards, RankNTypes, ConstraintKinds, GADTs, TypeSynonymInstances, FlexibleInstances, QuantifiedConstraints, MultiParamTypeClasses #-}
module Chopaan.Hydrate
  ( runHydration
  , Command(..)
  , onCommand
  , unfoldNodes
  , LifeTime(..)
  , HydrationConf(..)
  , parseHConf
  , mkTKbtz
  , ufStream
  , prefixGen
  , nodePrefixes
--  , congregatePrefixes
  , latestPrefix
  , mapToStream
  , HConM
  , HConS
  ) where


import Chopaan.Hydration.Prefix
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
import Chopaan.Graph

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
import Data.IORef (readIORef)
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
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import Network.AWS hiding (Metadata)
import Network.AWS.Data.Sensitive (Sensitive(..))
import qualified Network.AWS.S3 as S3

import Network.DNS.Resolver
import qualified Network.DNS.Cache as NC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types as NC
import qualified Network.HTTP.Client as NC
import qualified Network.HTTP.Client.Internal as NC (hostAddress)

import Database.InfluxDB.Line (Line)
import qualified Database.InfluxDB.Query as Q
import qualified Database.InfluxDB.Write.UDP as Udp
import qualified Database.InfluxDB.Write as Http
import qualified Database.InfluxDB.Format as F
import qualified Database.InfluxDB.Manage as DB
import qualified Database.InfluxDB.Types as DB

import qualified Codec.Winery as W
import Data.ProtoLens

import Data.Void

import qualified Network.Wreq.Session as Session
import Network.Wreq

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
import System.Envy hiding (env)



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


data ManagerSettings = ManagerSettings
  { manConnCount :: Int
  , manIdleConn :: Int
  , manTimeout :: Int
  } deriving (Generic, FromEnv)

data ParStrategy = ParStrategy
  { dlThreads :: Int
  , sourceGenThreads :: Int
  } deriving (Generic, FromEnv)

parseParStrategy :: IO ParStrategy
parseParStrategy = decodeWithDefaults (ParStrategy 1000 5)

parseHConf :: IO (HydrationConf)
parseHConf = decodeWithDefaults hConfDef

parseHConf' :: IO (Either String HydrationConf)
parseHConf' = decodeEnv

parseManagerConf :: IO (ManagerSettings)
parseManagerConf = decodeWithDefaults manConfDef

manConfDef :: ManagerSettings
manConfDef = ManagerSettings 1000 128 90

hConfDef :: HydrationConf
hConfDef = HydrationConf basePath bucket defDate Ten5 Ten2 Infinite 
  where
    defDate = Date 1 1 2021
    basePath = "./data/hydration"
    bucket = "dosti-datastream"

data KbtzConf = KbtzConf
  { env :: Env
  , bucket :: S3.BucketName
  , kbtzStore :: KbtzStore
  , kbtzName :: KbtzName
  , manOrSesh :: Either NC.Manager Session.Session
  , startTime :: Time.UTCTime
  , life :: LifeTime
  , writeParams :: Http.WriteParams
  , res :: (Resolution, Resolution)
  , parHow :: ParStrategy
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

mkTKbtz :: [(KbtzName, [NodeMAC])] -> STM (TKbtzim)
mkTKbtz kns = do
  nSets <- traverse (newTVar . Set.fromList) (snd <$> kns)
  newTVar (M.fromList (zip (fst <$> kns) nSets))

mkKbtzConf :: HydrationConf
  -> Env
  -> KbtzName
  -> KbtzStore
  -> Either NC.Manager Session.Session
  -> Http.WriteParams
  -> ParStrategy
  -> KbtzConf
mkKbtzConf (HydrationConf{s3Bucket
                         , startDate, lifetime
                         , pastRes, futureRes}) env name store manOrSesh wp parHow =
  KbtzConf env (S3.BucketName s3Bucket) store name manOrSesh (toUTC startDate) lifetime wp (pastRes, futureRes) parHow

runHydration :: TinkerConf -> HydrationConf -> IO ()
runHydration tk conf = do
  kns <- runGraphM (PoolConf 1 1 1) tk getKNs
  manConf <- parseManagerConf
  sesh <- liftIO $ Session.newSessionControl Nothing (ourSettings manConf)
  parConf <- parseParStrategy
  kbtzim <- atomically $ mkTKbtz kns
  aws <- getAwsEnv S3.s3
  man <- newManager manConf
  let hydrationDB = "chopaanS3"
  let p = DB.queryParams hydrationDB
  DB.manage p $ F.formatQuery ("CREATE DATABASE "F.%F.database) hydrationDB
  let
    configureH (kId, kNodes) = do
      let
        store = initKbtzStore kId (storePath conf)
        c = mkKbtzConf conf aws kId store (Right sesh) (wp hydrationDB) parConf 
      hydrateKbtz c kNodes (unfoldNodes (lifetime conf) kbtzim)
  S.drain . S.fromWAsync $
    S.mapM configureH $ S.unfold (unfoldKbtzim (lifetime conf) kbtzim) ()

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
unfoldNodes lt tv = -- traceUF (liftIO . print) $ 
  UF.many (UF.mkUnfoldM step inject) UF.fromList
  where
    delS = 10
    delay = liftIO $ threadDelay $ round $ delS * 1000000
    onNullDiff s = case lt of
      Finite -> return UF.Stop
      Infinite -> delay >> return (UF.Skip s)
    step :: (KbtzName, Set NodeMAC) -> m (UF.Step (KbtzName, Set NodeMAC) [NodeMAC])
    step (k, oldSet) = do
      newSet <- liftIO . atomically $ lookupTSet k tv
      case newSet of
        Nothing -> return $ UF.Stop
        Just s -> do
          let diff = Set.difference s oldSet
          case (null diff) of
            True -> onNullDiff (k, s)
            False -> return $ UF.Yield (Set.toList diff) (k, s)
    inject :: KbtzName -> m (KbtzName, Set NodeMAC)
    inject k = return (k, mempty)

getKeys :: (Ord k) => TMap k v -> STM (Set k)
getKeys = (fmap (Set.fromList . M.keys)) . readTVar
    
unfoldKbtzim :: forall m. (HConM m) => LifeTime -> TKbtzim -> UF.Unfold m () (KbtzName, TNodes)
unfoldKbtzim lt tv = traceUF (liftIO . print . fst) $
                  UF.many (UF.mkUnfoldM step inject) UF.fromList
  where
    delS = 10
    delay = liftIO $ threadDelay $ round $ delS * 1000000
    onNullDiff s = case lt of
      Finite -> return UF.Stop
      Infinite -> delay >> return (UF.Skip s)
    step :: (Set KbtzName) -> m (UF.Step (Set KbtzName) [(KbtzName, TNodes)])
    step oldSet = do
      newSet <- liftIO . atomically $ getKeys tv
      let diff = Set.difference newSet oldSet
      case (null diff) of
        True -> onNullDiff newSet
        False -> do
          let z = (Set.toList diff)
          ps <- liftIO . atomically $ traverse (flip nodeSet' tv) z
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
hydrateKbtz KbtzConf{kbtzName, kbtzStore, manOrSesh, res, startTime, bucket, env, writeParams, life, parHow} tNodes ns = do
  t0 <- liftIO $ Time.getCurrentTime
  let
    prefixes' = ufStream (prefixGen life inSet res startTime t0)
    nps = nodePrefixes kbtzName (mkNodeDirs kbtzStore) ns prefixes'
  void $ S.fold (inFrame kbtzStore writeParams)
    $ S.fromAhead
    $ S.map (fst)
    $ S.filter ((> 0) . snd)
    $ S.trace (pr . frameLog)
    S.|$ dlFramesParFS parHow manOrSesh (bucket, env, t0) (getKbtzPath kbtzStore)
    $ S.fromWAsync
    $ S.map fst
    $ S.filter ((> 0) . snd)
    $ S.trace (pr . keyLog)
    S.|$ getKeysUF env bucket nps (fileSaver Keys kbtzStore)
  where
    inSet n = liftIO @m . atomically $ do
      s <- readTVar tNodes
      return $ Set.member n s
    {-# INLINE inSet #-}
    pr = liftIO . print
    prefLog (n, p) = "Prefix Generated :" <> (show (n, p))
    keyLog ((n, p), i) = "Keys Downloaded" <> (show (n, p, i))
    frameLog ((n, p), i) = "Frames Downloaded:" <> (show (n, p, i))

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
    past = S.fromList (prefixRange pastRes start (Just now))
    {-# INLINE past #-}
    future = posthence futureRes
    {-# INLINE future #-}
{-# INLINE prefixGen #-}
    

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

nodeDirUF :: forall m. HConM m => KbtzStore -> StoreType -> UF.Unfold m NodeMAC Word8
nodeDirUF store stage = uf
  where
    ps :: UF.Unfold m NodeMAC FilePath
    ps = UF.lmap (getKbtzFolder store stage) Dir.readFiles
    uf :: UF.Unfold m NodeMAC Word8
    uf = UF.many ps File.read


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

latestPrefix :: (HConM m) => A.Array Prefix -> m (Maybe Prefix)
latestPrefix a = A.fold (FL.maximum) a

-- congregatePrefixes :: HConM m => FL.Fold m (NodeMAC, Prefix) (M.Map NodeMAC (A.Array Prefix))
-- congregatePrefixes = --- FL.lmapM (\a -> (liftIO . print $ a) >> (return a)) $
--   --  FL.rmapM (\a -> (liftIO . print $ a) >> (return a))
--   -- (FL.demuxDefault mempty ((FL.lmap snd A.write)))


mapToStream :: (HConS t m, Ord k) => (k -> ar -> m (Maybe pi)) -> M.Map k ar -> m (t m (k, pi))
mapToStream f = (fmap (S.fromList
                       . map (second fromJust)
                       . filter (isJust . snd)
                       . M.toList))
                . (M.traverseWithKey f)

dlFramesParFS ::
  forall t m. HConS t m
  => ParStrategy
  -> Either NC.Manager Session.Session
  -> SignWith
  -> (StoreType -> NodeMAC -> Prefix -> FilePath)
  -> t m (NodeMAC, Prefix)
  -> t m ((NodeMAC, Prefix), Int)
dlFramesParFS st manOrSesh sw getPath = S.maxThreads (sourceGenThreads st) . S.mapM (uncurry dlF') --  $ --S.trace (liftIO . print) $ S.foldMany congregatePrefixes ps
  where
    dler = case manOrSesh of
      Left man -> dlHttpClient (dlThreads st) man sw
      Right sesh -> dlWreq (dlThreads st) sesh sw
    dlF' :: NodeMAC -> Prefix -> m ((NodeMAC, Prefix), Int)
    dlF' n p = dler (n, p) frameSink errSink keySource
      where
        frameSink = saveWithLength (n, p) (encodeFold (getPath Frames n p))
        errSink = encodeFold (getPath Errors n p)
        keySource :: S.AheadT m (S3Key)
        keySource = decodeKeys readAll
          where
            readAll = S.unfold File.read (getPath Keys n p)
            decodeKeys :: S.AheadT m Word8 -> S.AheadT m (S3Key)
            decodeKeys = S.map (fromWino)
              . S.rights
              . S.trace (logEither)
              . decodeS @S.AheadT @m @(Wino S3Key)
    -- dlM = mapToStream dlF
    -- dlF :: NodeMAC -> A.Array Prefix -> m (Maybe (Prefix, Int))
    -- dlF n s = do
    --   px <- latestPrefix s
    --   case px of
    --     Nothing -> return Nothing
    --     Just comPref -> do
    --       liftIO $ print $ "Starting Download for: " <> (show n) <> (show s)
    --       liftIO $ print $ "Common Prefix: " <> (show comPref)
    --       r <- download man sw (n, comPref) (frameSink comPref) (errSink comPref) keySource
    --       return $ Just r
    --   where
    --     frameSink p = saveWithLength p (encodeFold (getPath Frames n p))
    --     errSink p = encodeFold (getPath Errors n p)
    --     keySource :: forall t1. (S.IsStream t1) => t1 m (S3Key)
    --     keySource = decodeKeys (S.unfold readAll s)
    --       where
    --         readAll = UF.many ((getPath Keys n) <$> A.read) File.read
    --         decodeKeys :: t1 m Word8 -> t1 m (S3Key)
    --         decodeKeys = S.map (fromWino)
    --           . S.rights
    --           . S.trace (logEither)
    --           . decodeS @t1 @m @(Wino S3Key)
{-# INLINE dlFramesParFS #-}

saveWithLength :: (MonadIO m) => b -> FL.Fold m a () -> FL.Fold m a (b, Int) 
saveWithLength tag f = FL.rmapM (\x -> (return (tag, (snd x))))
                       (FL.tee f FL.length)
{-# INLINE saveWithLength #-}

dlHttpClient :: forall m tag r. (HConM m, Show tag)
  => Int
  -> NC.Manager
  -> SignWith
  -> tag
  -> FL.Fold m (Wino S3Body) r
  -> FL.Fold m (Wino GetObjError) ()
  -> S.AheadT m S3Key
  -> m r 
dlHttpClient nThreads man signWith = download (dl nThreads man signWith)
{-# INLINABLE dlHttpClient #-}


dlWreq :: forall m tag r. (HConM m, Show tag)
  => Int
  -> Session.Session
  -> SignWith
  -> tag
  -> FL.Fold m (Wino S3Body) r
  -> FL.Fold m (Wino GetObjError) ()
  -> S.AheadT m S3Key
  -> m r 
dlWreq nThreads sesh signWith = download (sessionS3 nThreads signWith sesh)
{-# INLINABLE dlWreq #-}


download :: forall m tag r. (HConM m, Show tag)
  => (S.AheadT m S3Key -> S.AheadT m S3Resp)
  -> tag
  -> FL.Fold m (Wino S3Body) r
  -> FL.Fold m (Wino GetObjError) ()
  -> S.AheadT m S3Key
  -> m r
download download' tag saveDL saveErr = (fmap snd)
  . S.fold (FL.partition saveErr saveDL)
  . S.map (bimap toWino toWino)
  . S.tapRate 60 (printDLRate)
  . S.fromAhead
  . download'
  where
    printDLRate r = liftIO . print
      $ "Download rate from Node "
      <> (show tag)
      <> " is " <> (show r)
{-# INLINE download #-}


dl :: forall m. HConM m => Int -> NC.Manager -> SignWith -> S.AheadT m S3Key -> S.AheadT m S3Resp
dl nThreads man signWith = S.trace (logEither)
                           . S.maxThreads nThreads
                           . S.mapM (goIdxd)
                           . S.mapM (signIdxd)
                           . S.maxRate (realToFrac $ nThreads * 2)
  where
    goIdxd :: S3Req -> m (S3Resp)
    goIdxd (S3Idx (idx, bs)) = do
      o <- getObject man bs
      return $ bimap (S3Idx . (idx,)) (S3Idx . (idx,)) o
    {-# INLINE goIdxd #-}
    signIdxd :: S3Key -> m (S3Req)
    signIdxd = traverse (signGetObject signWith)
    {-# INLINE signIdxd #-}
{-# INLINE dl #-}

type SignWith = (S3.BucketName, Env, Time.UTCTime)



sessionS3 :: forall m. (S.MonadAsync m, MonadMask m)
  => Int -> SignWith -> Session.Session -> S.AheadT m S3Key -> S.AheadT m S3Resp
sessionS3 threads (S3.BucketName bucket, env, _) sesh ks = S.concatM $ do
  aut <- liftIO $ getAuth
  let
    (AccessKey accessKey) = _authAccess aut
    (SecretKey secretKey) = desensitise . _authSecret $ aut
    opts = ropts accessKey secretKey
  return $ S.maxThreads threads
    $ S.mapM (pure . a) S.|$ S.mapM (traverse (s3GetSafe opts sesh))
    $ S.maxRate (1000) ks
  where
    a (S3Idx (idx, o)) = bimap (S3Idx . (idx,)) (S3Idx . (idx,)) o
    s3GetSafe opts sesh = recoverWith ("req" :: String) 3 (Left . wrapStatus $ NC.imATeapot418) . s3Get opts sesh
    s3Get :: Network.Wreq.Options
      -> Session.Session
      -> S3.ObjectKey
      -> m (Either RespStatus BS.ByteString)
    s3Get opts sesh r = liftIO $ do
      (checkResponse) =<< (Session.getWith opts sesh . toReq $ r) 
    region = "ap-southeast-1"
    basePath = T.unpack $ "https://" <> bucket <> ".s3." <> region <> ".amazonaws.com" <> "/"
    toReq (S3.ObjectKey k) = basePath <> (T.unpack k) 
    -- region = env ^. envRegion
    ropts access secret = defaults &
      auth ?~ awsFullAuth AWSv4 access secret Nothing (Just ("s3", "ap-southeast-1"))
    getAuth = case (env ^. envAuth) of
      Ref _ r -> readIORef r
      Auth ae -> return ae
    checkResponse :: NC.Response BL.ByteString -> IO (Either RespStatus (BS.ByteString)) 
    checkResponse resp = case (NC.responseStatus resp == NC.ok200) of
                           True -> do
                             let bo = BL.toStrict (NC.responseBody $ resp)
                             return . Right $! bo
                           _ -> return . Left . wrapStatus $ NC.responseStatus resp

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
  --  let r' = r{ NC.secure = False, NC.port = 80}
  recoverWith ("req" :: String) 3
    (Left . wrapStatus $ NC.imATeapot418)
    (NC.withResponse r manager checkResponse)
  where
    checkResponse :: NC.Response NC.BodyReader -> IO (Either RespStatus (BS.ByteString)) 
    checkResponse resp = case (NC.responseStatus resp == NC.ok200) of
                           True -> do
                             bo <- BS.concat <$> (NC.brConsume . NC.responseBody $ resp)
                             return . Right $ bo
                           _ -> return . Left . wrapStatus $ NC.responseStatus resp
{-# INLINE getObject #-}


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
    -- logPrefGen p = liftIO . print $ "prefix: " <> (show p)
    prefixKeys :: NodeMAC -> Prefix -> m r
    prefixKeys n t = do
      -- liftIO . print $ ("starting ting" <> (show (n, t)))
      r <- ((UF.fold
              (prefixFold n t)
              (UF.map (toS3Idx . (toS3Id &&& id) . fst) (s3Paths'' env (req n)))) t)
      -- liftIO . print $ ("finished writing keys" <> (show (n, t, r)))
      --recoverC (show n <> " " <> show t) 3 
      return $ r
      where
        -- FL.lmapM (\a -> (liftIO . print $ a) >> (return a)) $ 
        req n' prefix = S3.listObjectsV2 bucket & S3.lovPrefix .~ (timedPrefix n' prefix)
{-# INLINE getKeysUF #-}

ourSettings :: ManagerSettings -> NC.ManagerSettings
ourSettings ManagerSettings{..} = cachingSettings
  where
    oneSec = 1000000
    cachingSettings = tlsManagerSettings -- -- NC.defaultManagerSettings --
      { NC.managerConnCount = manConnCount
      , NC.managerIdleConnectionCount = manIdleConn 
      , NC.managerResponseTimeout = NC.responseTimeoutMicro (manTimeout * oneSec)
      -- , NC.managerModifyRequest = preResolveReq c  
      }


newManager :: (MonadIO m) => ManagerSettings -> m NC.Manager
newManager ms = liftIO $ NC.newManager (ourSettings ms) 


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




