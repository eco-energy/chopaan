{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeApplications, ScopedTypeVariables, ExplicitForAll, NamedFieldPuns, TupleSections, BangPatterns, PolyKinds, DataKinds, UnboxedTuples #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable, DerivingVia, CPP, LambdaCase, RecordWildCards, RankNTypes, ConstraintKinds, GADTs #-}
module Main where

import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId
import Chopaan.Comm.S3 hiding (pathFile)
import Chopaan.Types
import Streamly.Binary
import Chopaan.Kibbutz.AWS.Common hiding (preResolvingManager)
import Chopaan.Utils.Retry (recoverC, recoverOrNothing, recoverWith)
import Chopaan.Comm.Dispatch (accessEnergyState, accessRTS)
import Chopaan.Node.Folds (sensorFold, meshFold)
import Chopaan.Utils.Time (utcTimeNow)
import Data.Influxable (KbtzNode, asKbtzNode, lineSensorR
                       , lineMesh, lineFoldHttp, showText, chopaanDB, wp)


import Proto.NodeMessageSchema.NodeMessages (MeshFrame, EnergyState, RuntimeStats)
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N

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
import qualified Streamly.Internal.Data.Array.Stream.Foreign as A
import qualified Streamly.Internal.Data.Time.Units as ST
import Streamly.Internal.Data.IORef.Prim (Prim(..))
import Dhall hiding (newManager, void)
import System.Directory

#if 1
import Paths_chopaan
#else
getDataFileName = error "goob"
#endif

getOpts = do
  path <- getDataFileName "hydration.dhall"
  input auto $ T.pack path

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

type HConM m = (S.MonadAsync m, MonadCatch m, MonadThrow m)

type HConS t m = (S.IsStream t, HConM m) 

type HConfM m a = ReaderT (DLConf m) m a

data DLConf m = DLConf
  { env :: Env
  , bucket :: S3.BucketName
  , kbtzStore :: KbtzStore m
  , kbtzName :: KbtzName
  } deriving (Generic)


main :: IO ()
main = runHydration


type TNodes = TVar (Set NodeMAC)

type TKbtzim = TVar (M.Map KbtzName TNodes)

getNodes :: KbtzName -> TKbtzim -> STM (Maybe (Set NodeMAC))
getNodes k tv = do
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



runHydration :: IO ()
runHydration = do
  kbtzim <- atomically $ do
    ns <- newTVar (Set.fromList labNodes)
    newTVar (M.fromList [(kbtzId, ns)])
  let control = S.mapM (atomically . onCommand kbtzim) parseCmd
  tNow <- Time.getCurrentTime
  hOpts <- getOpts
  aws <- getAwsEnv S3.s3
  let p = DB.queryParams chopaanDB
      basePath = "./data/hydration"
  DB.manage p $ F.formatQuery ("CREATE DATABASE "F.%F.database) chopaanDB
  let h = S.fromEffect (hydrateKbtz hOpts basePath aws chopaanDB kbtzId kbtzim)
  S.drain $ h `S.wAsync` control
  where
    kbtzId = KbtzId "Lab_TestGrid"

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


getKbtzNodes :: forall m. (HConM m) => TKbtzim -> UF.Unfold m KbtzName NodeMAC
getKbtzNodes tv = UF.many (UF.mkUnfoldM step inject) UF.fromList
  where
    step :: (KbtzName, Set NodeMAC) -> m (UF.Step (KbtzName, Set NodeMAC) [NodeMAC])
    step (k, oldSet) = liftIO . atomically $ do
      newSet <- getNodes k tv
      case newSet of
        Nothing -> do
          --print $ "No New Set, Stopping: " <> (show k)
          --print (oldSet)
          return $ UF.Stop
        Just s -> do
          --print $ "New Set Exists for: " <> (show k) <> " " <> (show s)
          let diff = Set.difference s oldSet
          --print $ "Diff Is: " <> (show diff)
          case (null diff) of
            True -> do
              retry
              --print $ "Skipping " <> (show k)
              --return $ UF.Skip (k, s)
            False -> do
              --print $ "Yielding " <> (show k)
              return $ UF.Yield (Set.toList diff) (k, s)
    inject :: KbtzName -> m (KbtzName, Set NodeMAC)
    inject k = return (k, mempty)

hydrateKbtz :: forall m. (HConM m)
  => HydrationOpts
  -> FilePath
  -> Env
  -> DB.Database
  -> KbtzName
  -> TKbtzim
  -> m ()
hydrateKbtz hOpts basePath aws chopaanDB kbtzId kbtzim = do
  t0 <- liftIO $ Time.getCurrentTime
  let ns = getKbtzNodes kbtzim
  nodeSet <- liftIO . atomically $ do
    nodeSet' <- (M.lookup kbtzId) <$> readTVar kbtzim
    case nodeSet' of
      Nothing -> retry
      Just s -> return s
  store <- initKbtzStore kbtzId basePath ns (prefs nodeSet t0 hOpts)
  dlPaths <- (flip runReaderT) (DLConf aws bucket store kbtzId) $ do
    keyPaths <- S.trace (pr . keyLog) <$> getKeysFS @S.AheadT
    S.trace (pr . frameLog) <$> dlFramesParFS keyPaths
  S.drain . S.trace (pr . ingestLog) . S.fromAhead $
    inFrame store (Http.writeParams chopaanDB) dlPaths
    where
      pr = liftIO . print
      keyLog (n, p) = "Keys Downloaded for Node :" <> (show n) <> " and Prefix :" <> (show p)
      frameLog (n, p) = "Frames Downloaded for Node :" <> (show n) <> " and Prefix :" <> (show p)
      ingestLog = const "Frames Ingested"
      bucket = S3.BucketName $ s3BucketName hOpts
      prefs nodeSet to HydrationOpts{start, resolution} =
        UF.many (UF.function (prefixGen @m nodeSet resolution (toUTC start) to)) UF.fromStream


prefixGen :: (S.MonadAsync m, Ord a)
  => TVar (Set a) -> Resolution -> Time.UTCTime -> Time.UTCTime -> a -> S.SerialT m Prefix 
prefixGen tv r start now n = past <> future
  where
    inSet = liftIO . atomically $ do
      s <- readTVar tv
      return $ Set.member n s
    {-# INLINE inSet #-}
    past = S.fromList p'
    {-# INLINE past #-}
    future = S.takeWhileM (\_ -> inSet) $ S.delayPre (fromIntegral delF)
      (S.unfold UF.enumerateFromStepIntegral (last p', (Prefix delF)))
    {-# INLINE future #-}
    p' = prefixRange Minute start now
    delF = ceiling . (10 **) . realToFrac . (ceiling . logBase 10 . resDiff) $ Minute
    {-# INLINE delF #-}
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

data KbtzStore m = KbtzStore
  { nodes :: UF.Unfold m Void NodeMAC
  , prefixes :: UF.Unfold m NodeMAC Prefix
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



getKbtzFolder :: KbtzStore m -> StoreType -> (NodeMAC -> FilePath)
getKbtzFolder KbtzStore{..} = \case
  Keys -> keyFolder
  Frames -> frameFolder
  Errors -> errFolder
  Ingested -> ingestedFolder

getKbtzPath :: KbtzStore m -> StoreType -> (NodeMAC -> Prefix -> FilePath)
getKbtzPath KbtzStore{..} = \case
  Keys -> keyFile
  Frames -> frameFile
  Errors -> errFile
  Ingested -> ingestedFile

    
traceUF :: (Monad m) => (a -> m b) -> UF.Unfold m x a -> UF.Unfold m x a
traceUF f = UF.mapM (\a -> f a >> (pure a))

initKbtzStore :: forall m. (MonadIO m) => KbtzName -> FilePath -> UF.Unfold m KbtzName NodeMAC -> UF.Unfold m NodeMAC Prefix -> m (KbtzStore m)
initKbtzStore kbtz base kns ps = do
  return $ KbtzStore (traceUF mkNodeDirs ns) ps
    (hFolder Keys) (hFolder Frames) (hFolder Errors) (hFolder Ingested)
    (hFile Keys) (hFile Frames) (hFile Errors) (hFile Ingested) (asKbtzNode kbtz)
  where
    ns = UF.supply kbtz kns
    root = base </> (T.unpack . showText $ kbtz)
    nodeDirHere here n =  root </> here </> (nodeMACPath n)
    mkNodeDirHere p n = liftIO $ cd (nodeDirHere p n)
    mkNodeDirs :: NodeMAC -> m ()
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


ingestKbtzFrames :: HConM m
  => KbtzStore m
  -> Http.WriteParams
  -> m ()
ingestKbtzFrames store writeParams = S.drain
  $ S.fromParallel
  $ S.mapM (ingestNodeFramesFS store fl) $ S.unfold0 (nodes store)
  where
    fl = lineFoldHttp 32 writeParams

nodeDirUF :: forall m. HConM m => KbtzStore m -> StoreType -> UF.Unfold m NodeMAC Word8
nodeDirUF store stage = uf
  where
    ps :: UF.Unfold m NodeMAC FilePath
    ps = UF.lmap (getKbtzFolder store stage) Dir.readFiles
    uf :: UF.Unfold m NodeMAC Word8
    uf = UF.many ps File.read


ingestNodeFramesFS :: forall m. (HConM m) => KbtzStore m -> FL.Fold m [Line Time.UTCTime] () -> NodeMAC -> m ()  
ingestNodeFramesFS store lnFold n =
  ingestFrames (nodeDirUF store Frames) lnFold (tagger store) n


inFrame :: forall t m. (HConS t m)
  => KbtzStore m
  -> Http.WriteParams
  -> t m (NodeMAC, Prefix)
  -> t m ()
inFrame store writeParams s = S.mapM
  (\(n, p) -> ingestFrames (reader p) fl (tagger store) n) s
  where
    reader p = UF.many (UF.function (\n' -> getKbtzPath store Frames n' p)) File.read
    fl = lineFoldHttp 32 writeParams

ingestFrames :: forall m r. (S.MonadAsync m, MonadCatch m)
  => UF.Unfold m NodeMAC Word8
  -> FL.Fold m [Line Time.UTCTime] r
  -> (NodeMAC -> KbtzNode)
  -> NodeMAC
  -> m r
ingestFrames source lnFold tag n = do
  S.fold lnFold
    . S.fromAhead
    . S.maxThreads 3000
    . S.tapRate 10 (\r -> liftIO $ print $ "Ingest Rate for " <> (show n) <> " : " <> (show r))
    . S.postscan (nodeFold)
    . S.catMaybes
    . S.trace (logNothing)
    . S.mapM (pure . validateMF')
    . S.rights
    . S.trace (logEither)
    . S.map parsePB
    . decodeFrames
    $ S.unfold source n
  where
    nodeFold = fmap nodeLines (FL.partition sensorFold (meshFold n))
    nodeLines = (\(a, b) -> a <> b) . bimap (lineSensorR (tag n)) (lineMesh (tag n))
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

kbtzFramesFS :: forall m. HConM m
  => HConfM m ()
kbtzFramesFS = do
  DLConf{kbtzStore, env, bucket} <- ask
  lift $ do
    time <- liftIO $ Time.getCurrentTime
    let sw = (bucket, env, time)
    let ns = nodes kbtzStore
        ps = prefixes kbtzStore
    S.drain
      . S.fromWAsync
      . S.mapM (\(n, man) -> downloadNodeFS man kbtzStore sw ps n)
      $ S.unfold0 (UF.mapM (\n -> (n,) <$> (newManager)) ns) 
{-# INLINE kbtzFramesFS #-}


dlFramesParFS ::
  forall t m. HConS t m => t m (NodeMAC, Prefix)
  -> HConfM m (t m (NodeMAC, Prefix))
dlFramesParFS paths = do
  DLConf{kbtzStore, env, bucket} <- ask
  lift $ do
    time <- liftIO $ Time.getCurrentTime
    let sw = (bucket, env, time)
    man <- newManager
    let
      dlF :: (NodeMAC, Prefix) -> m (NodeMAC, Prefix)
      dlF (n, s) = download man sw (n, s) frameSink errSink keySource 
          where
            frameSink = fmap fst (FL.tee
                                  (FL.fromPure (n, s))
                                  (encodeFold (getKbtzPath kbtzStore Frames n s)))
            errSink = encodeFold (getKbtzPath kbtzStore Errors n s)
            keySource :: forall t1. (S.IsStream t1) => t1 m (S3Key)
            keySource = loadFile (getKbtzPath kbtzStore Keys n s)
    return $ S.maxThreads 10 $ S.mapM dlF paths
{-# INLINE dlFramesParFS #-}

downloadNodeFS :: forall m. HConM m
  => NC.Manager
  -> KbtzStore m
  -> SignWith
  -> UF.Unfold m NodeMAC Prefix
  -> NodeMAC
  -> m () 
downloadNodeFS man store sw ps n = S.drain
    $ S.fromWAsync
    $ S.mapM (\p -> (downloadFS man sw (keySource p) (frameSink p) (errSink p) n))
    $ S.unfold ps n 
  where
    frameSink = (getKbtzPath store Frames n)
    errSink = (getKbtzPath store Errors n)
    keySource = (getKbtzPath store Keys n)
{-# INLINE downloadNodeFS #-}

downloadFS :: forall m a. (HConM m, Show a)
  => NC.Manager -> SignWith -> FilePath -> FilePath -> FilePath -> a -> m ()
downloadFS man sw sourceKey sinkFrame sinkErr tag =
  download man sw tag (encodeFold sinkFrame) (encodeFold sinkErr) (loadFile sourceKey)
{-# INLINE downloadFS #-}

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
  . S.tapRate 10 (printDLRate)
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
                  . S.maxThreads 5500
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
  recoverWith "req" 4
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

getKeysFS :: forall t m. (HConS t m) => HConfM m (t m (NodeMAC, Prefix))
getKeysFS = do
  store <- kbtzStore <$> ask
  getKeys (toFileCount store)
  where
    toFileCount :: KbtzStore m -> NodeMAC -> Prefix -> FL.Fold m S3Key (NodeMAC, Prefix) 
    toFileCount store n t = fmap fst (FL.tee (FL.fromPure (n, t)) (FL.lmap toWino (encodeFold path)))
      where
        path = (getKbtzPath store Keys n t)
{-# INLINE getKeysFS #-}


getKeys :: forall t m r. (HConS t m)
  => (NodeMAC -> Prefix -> FL.Fold m S3Key r) -> HConfM m (t m r)
getKeys prefixFold = do
  DLConf{env, bucket, kbtzStore} <- ask
  let ns = nodes kbtzStore
      ps = prefixes kbtzStore
  lift $ return $ getKeysUF env bucket ns ps prefixFold
{-# INLINE getKeys #-}


getKeysUF :: forall t m r. (HConS t m)
  => Env
  -> S3.BucketName
  -> UF.Unfold m Void NodeMAC
  -> UF.Unfold m NodeMAC Prefix
  -> (NodeMAC -> Prefix -> FL.Fold m S3Key r)
  -> t m r
getKeysUF env bucket ns ps prefixFold = S.mapM (uncurry prefixKeys)
                        $ S.unfoldManyRoundRobin (UF.mapMWithInput (\a b -> pure (a,b)) ps)
                        $ S.unfold0 ns
  where
    logPrefGen p = liftIO . print $ "prefix: " <> (show p)
    prefixKeys :: NodeMAC -> Prefix -> m r
    prefixKeys n t = (UF.fold
                      (prefixFold n t)
                      (UF.map (toS3Idx . (toS3Id &&& id) . fst) (s3Paths'' env (req n)))) t
      where
        req n' prefix = S3.listObjectsV2 bucket & S3.lovPrefix .~ (timedPrefix n' prefix)
{-# INLINE getKeysUF #-}


decodeKeys :: forall t m. HConS t m
                => t m Word8 -> t m S3Key
decodeKeys = (S.map fromWino) . S.rights . (S.trace logEither) . decodeS 
{-# INLINE decodeKeys #-}

loadPrefixKeys :: forall t m. HConS t m
                => KbtzStore m -> NodeMAC -> Prefix -> t m S3Key
loadPrefixKeys store n = loadFile . getKbtzPath store Keys n
{-# INLINE loadPrefixKeys #-}

loadPrefixFrames :: forall t m. HConS t m
                => KbtzStore m -> NodeMAC -> Prefix -> t m S3Body
loadPrefixFrames store n = loadFile . getKbtzPath store Frames n
{-# INLINE loadPrefixFrames #-}

loadPrefixErrors :: forall t m. HConS t m
                => KbtzStore m -> NodeMAC -> Prefix -> t m S3Body
loadPrefixErrors store n = loadFile . getKbtzPath store Errors n
{-# INLINE loadPrefixErrors #-}

loadFile :: forall t m a. (HConS t m, W.Serialise a)
                => FilePath -> t m a
loadFile path = S.map (fromWino)
      $ S.rights
      $ S.trace (logEither)
      $ decodeFile path
{-# INLINE loadFile #-}


newManager :: (MonadIO m) => m NC.Manager
newManager = liftIO $ NC.newManager cachingSettings
  where
    cachingSettings = tlsManagerSettings
      { NC.managerConnCount = 2048
      , NC.managerIdleConnectionCount = 2024
      , NC.managerResponseTimeout = NC.responseTimeoutMicro (60 * oneSec)
      --, NC.managerModifyRequest = preResolveReq c  
      }
      where
        oneSec = 1000000
    preResolveReq cache r = do
      h <- liftIO $ NC.lookup cache (NC.host r)
      let r' = r { NC.hostAddress = h }
      return r'

cachingManager :: (MonadIO m) => NC.DNSCache -> m NC.Manager
cachingManager c = liftIO $ NC.newManager cachingSettings
  where
    cachingSettings = tlsManagerSettings
      { NC.managerConnCount = 2048
      , NC.managerIdleConnectionCount = 2024
      , NC.managerResponseTimeout = NC.responseTimeoutMicro (60 * oneSec)
      --, NC.managerModifyRequest = preResolveReq c  
      }
      where
        oneSec = 1000000
    preResolveReq cache r = do
      h <- liftIO $ NC.lookup cache (NC.host r)
      let r' = r { NC.hostAddress = h }
      return r'

cacheConf' :: NC.DNSCacheConf
cacheConf' = NC.DNSCacheConf
  { NC.resolvConfs = [
      defaultResolvConf
      ]
  , NC.maxConcurrency = 100
  , NC.minTTL = 30
  , NC.maxTTL = 60
  , NC.negativeTTL = 300
  }

logNothing :: forall m a. (MonadIO m, Show a) => Maybe a -> m ()
logNothing = \x -> if (isNothing x) then (liftIO $ print x) else (return ())
{-# INLINE logNothing #-}

logEither :: forall m a b. (MonadIO m, Show a) => Either a b -> m ()
logEither x = case x of
  Left a -> liftIO $ print a
  Right _ -> return ()
{-# INLINE logEither #-}




data Metadata = Metadata
  { mKeys :: !Int
  , mFrame :: !Int
  , mError :: !Int
  --, mLoad :: !Int
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (W.Serialise) via (W.WineryRecord Metadata) 


mkMetadata :: forall m. (S.MonadAsync m, MonadCatch m)
           => KbtzStore m -> NodeMAC -> Prefix -> m Metadata
mkMetadata s n p = Metadata
                   <$> (fileLen $ loadPrefixKeys s n p)
                   <*> (fileLen $ loadPrefixFrames s n p)
                   <*> (fileLen $ loadPrefixErrors s n p)
  where
    fileLen = S.fold FL.length



testComm :: IO (Bool)
testComm = do
  let ks = (KbtzId . T.pack . (pure @[])) <$> ['a'..'d']
  let ns = (NodeId . T.pack . show) <$> [1..12]
  let kns = fst $ foldr zop ([], ns) ks
        where
          zop :: k -> ([(k, [n])], [n]) -> ([(k, [n])], [n]) 
          zop k (k', n') = ((k, take 3 n') : k', drop 3 n')
  let addKs = (uncurry StartKbtz) <$> kns
      rmKs = StopKbtz <$> ks
      addNs = conc $ (\(k', ns') -> (StartNode k' <$> ns')) <$> kns
      rmNs = conc $ (\(k', ns') -> (StopNode k' <$> ns')) <$> kns
  k <- atomically $ newTVar mempty
  -- let readK tag = do
  --       print tag
  --       v <- atomically $ readKbtzim k
  --       print v
  kadd <- mapM_ (atomically . onCommand k) addKs
  p <- async $ do
    S.mapM_ (print) $ S.unfoldManyRoundRobin (getKbtzNodes k) (S.fromList ks)
    print ("STOPPING 1" :: String)
  --kadd <- mapM_ (atomically . onCommand k) addKs
  --readK "Add Ks"
  krm <- mapM_ (atomically . onCommand k) rmKs
  wait p
  --readK "Rm Ks"
  mapM_ (atomically . onCommand k) ((flip StartKbtz []) <$> ks)
  --readK "Add JUST kbtz"
  --p' <- async $ do
  --  S.mapM_ (print) $ S.unfoldManyRoundRobin (getKbtzNodes k) (S.fromList ks)
  --  print ("STOPPING 2" :: String)
  nadd <- mapM_ (atomically . onCommand k) addNs
  --readK "Add Ns"
  nrm <- mapM_ (atomically . onCommand k) rmNs
  mapM_ (atomically . onCommand k) rmKs
  --wait p'
  --readK "RM Ns"
  return True
  where
    conc = foldl (<>) mempty
