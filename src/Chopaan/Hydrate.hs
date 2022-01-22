{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeApplications, ScopedTypeVariables, NamedFieldPuns, TupleSections, BangPatterns, PolyKinds, DataKinds, UnboxedTuples #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveTraversable, DerivingVia, CPP, LambdaCase, RecordWildCards, RankNTypes, ConstraintKinds, GADTs, FlexibleInstances, QuantifiedConstraints, MultiParamTypeClasses, OverloadedLists, AllowAmbiguousTypes #-}
module Chopaan.Hydrate
  -- ( runHydration
  -- -- , Command(..)
  -- -- , onCommand
  -- , unfoldNodes
  -- , LifeTime(..)
  -- , HydrationConf(..)
  -- , parseHConf
  -- , mkTKbtz
  -- , TKbtzim
  -- , mkConfig
  -- , ufStream
  -- , prefixGen
  -- , nodePrefixes
  -- , HConM
  -- , HConS
  -- , hConfDef
  -- )
where


import Chopaan.Hydration.Prefix
    ( Resolution(..),
      Prefix(Prefix),
      posthence,
      prefixRange,
      asFileName )
import Chopaan.Node.NodeId ( NodeMAC, NodeId (..) )
import Chopaan.Kibbutz.KbtzId ( KbtzId(unKbtzId), KbtzName )
import qualified Chopaan.Kibbutz.FS as FS
import Chopaan.Kibbutz.FS (Kbtzim, NodeModel(..))
import Chopaan.Kibbutz.TKbtzim
    ( LifeTime(..),
      TKbtzim,
      TNodes,
      MACSet,
      mkTKbtz,
      mkConfig,
      unfoldNodes,
      unfoldKbtzim,
      toMACSet)
import Chopaan.Comm.S3
    ( timedPrefix,
      nodeMACPath,
      cd,
      unObject,
      fixGridTS,
      fixMeshTS )
import Chopaan.Types ( toUTC, Date(Date), InfluxConn )
import Streamly.Binary
    ( PB,
      Wino,
      HasEncoding(decodeA),
      toWino,
      fromWino,
      fromPB,
      decodeS,
      decodeFile,
      -- decodeUnfold,
      encodeFold )
import Chopaan.Kibbutz.AWS.Common ( Env, withAwsEnv, getAwsEnv, pageUFM )
import Chopaan.Utils.Retry (recoverWith)
import Chopaan.Comm.Dispatch (accessEnergyState, accessRTS)
import Chopaan.Node.Folds (sensorFold, meshFold, SensorR, MeshR)
import Chopaan.Node.HW

import Data.Influxable (KbtzNode, asKbtzNode, lineSensorR
                       , lineMesh, lineFoldHttp, showText)

import Proto.NodeMessageSchema.NodeMessages (MeshFrame, EnergyState, RuntimeStats)

import GHC.Generics ( Generic )

import Control.Arrow ((&&&))
import Control.Monad ( void, when, forever, (<=<) )
import Control.Monad.Trans.Class ()
import Control.Monad.Trans.Reader ( ReaderT, ask, runReaderT )
import Control.Monad.Catch ( MonadMask, MonadThrow, MonadCatch )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Bayes.Class ( MonadSample )
import Control.Monad.Bayes.Sampler ()
import Control.Lens ( (&), Bifunctor(bimap), (^.), (.~), (?~) )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ()
import Control.Concurrent.STM
    ( STM,
      TVar,
      atomically,
      retry,
      newTVar,
      readTVar,
      writeTVar,
      TQueue,
      modifyTVar,
      modifyTVar' )
import Control.Concurrent.STM.TVar ()
import Control.Concurrent.STM.TQueue ()
import Data.Bifunctor ( Bifunctor(second) )
import Data.Word ( Word8 )
import Data.IORef (readIORef)
import qualified Data.Map.Strict as M
import Data.Int ()
import Data.Maybe ( isJust, fromJust, fromMaybe, isNothing )
import Data.Either ( fromRight, isRight )

import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Data.Time as Time
import Data.Time.Clock.POSIX.Compat (posixSecondsToUTCTime)

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import Network.AWS
    ( Auth(Auth, Ref),
      HasEnv(envAuth),
      AccessKey(AccessKey),
      AuthEnv(_authAccess, _authSecret),
      SecretKey(SecretKey),
      presignURL,
      MonadAWS(liftAWS) )
import Network.AWS.Data.Sensitive (Sensitive(..))
import qualified Network.AWS.S3 as S3

import Network.DNS.Resolver ()
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types as NC
import qualified Network.HTTP.Client as NC

import qualified Database.InfluxDB.Write as Http
import qualified Database.InfluxDB.Format as F
import qualified Database.InfluxDB.Manage as DB

import qualified Codec.Winery as W
import Data.ProtoLens ()

import Data.Void ()

import qualified Network.Wreq.Session as Session
import Network.Wreq
    ( AWSAuthVersion(AWSv4), Options, defaults, awsFullAuth, auth )

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Refold.Type as RF
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.External.ByteString as SBS
import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Time.Units as ST
import qualified Streamly.Internal.Data.Sink as Sink
import qualified Streamly.Internal.FileSystem.Event.Linux as EvL
import Streamly.Internal.FileSystem.Event.Linux (Event(..), Toggle(..))
import Streamly.Internal.Data.IORef.Prim (Prim)
--import Dhall hiding (newManager, void)
import System.Directory (getFileSize, doesFileExist)
import System.Envy
    ( Var(..), FromEnv, decodeEnv, decodeWithDefaults )
import Path
import qualified Path.IO as PIO


type HConM m = (S.MonadAsync m, MonadCatch m, MonadThrow m, MonadMask m)

type HConS t m = (S.IsStream t, HConM m, Monad (t m))

type HConfM m a = ReaderT KbtzConf m a


data HydrationConf = HydrationConf
  { storePath :: !FilePath
  , s3Bucket :: !T.Text
  , startDate :: !Date
  , pastRes :: !Resolution
  , futureRes :: !Resolution
  , lifetime :: !LifeTime
  } deriving (Generic, FromEnv)


data ManagerSettings = ManagerSettings
  { manConnCount :: !Int
  , manIdleConn :: !Int
  , manTimeout :: !Int
  } deriving (Generic, FromEnv)

data ParStrategy = ParStrategy
  { dlThreads :: !Int
  , sourceGenThreads :: !Int
  } deriving (Generic, FromEnv)

--newtype HydrationM m a = HydrationM (ReaderT HydrationConf (StateT m) a)

parseParStrategy :: IO ParStrategy
parseParStrategy = decodeWithDefaults (ParStrategy 1000 5)

parseHConf :: IO HydrationConf
parseHConf = decodeWithDefaults hConfDef

parseHConf' :: IO (Either String HydrationConf)
parseHConf' = decodeEnv

parseManagerConf :: IO ManagerSettings
parseManagerConf = decodeWithDefaults manConfDef

manConfDef :: ManagerSettings
manConfDef = ManagerSettings 128 10 60

hConfDef :: HydrationConf
hConfDef = HydrationConf basePath bucket defDate Ten5 Ten Finite
  where
    defDate = Date 14 10 2021
    basePath = "./data/hydration"
    bucket = "dosti-datastream"

data KbtzConf = KbtzConf
  { env :: !Env
  , bucket :: !S3.BucketName
  , kbtzStore :: !KbtzStore
  , kbtzName :: !KbtzName
  , manOrSesh :: !Session.Session
  , startTime :: !Time.UTCTime
  , life :: !LifeTime
  , writeParams :: !Http.WriteParams
  , res :: !(Resolution, Resolution)
  , parHow :: !ParStrategy
  , ropts :: !Options
  } deriving (Generic)



mkKbtzConf :: HydrationConf
  -> Env
  -> KbtzName
  -> KbtzStore
  -> Session.Session
  -> Http.WriteParams
  -> ParStrategy
  -> Options
  -> KbtzConf
mkKbtzConf HydrationConf{s3Bucket
                         , startDate, lifetime
                         , pastRes, futureRes} env name store manOrSesh wp =
  KbtzConf env (S3.BucketName s3Bucket)
  store name
  manOrSesh (toUTC startDate)
  lifetime wp
  (pastRes, futureRes)

mkKbtzConfM :: forall m. (HConM m)
  => Http.WriteParams -> (KbtzName, TNodes) -> m (KbtzConf, TNodes)
mkKbtzConfM wp (kId, kNodes) = do
  !conf <- liftIO parseHConf
  !parConf <- liftIO parseParStrategy
  !aws <- getAwsEnv S3.s3
  !manConf <- liftIO parseManagerConf
  !basePath <- PIO.makeAbsolute =<< parseRelDir (storePath conf)
  -- TODO: Session should not be per kibbutz?
  !sesh <- liftIO $ Session.newSessionControl Nothing (ourSettings manConf)
  !aut <- liftIO $ do
    case aws ^. envAuth of
      Ref _ r -> readIORef r
      Auth ae -> return ae
  let
    (AccessKey accessKey) = _authAccess aut
    (SecretKey secretKey) = desensitise . _authSecret $ aut  
    ropts = defaults &
      auth ?~ awsFullAuth AWSv4 accessKey secretKey Nothing (Just ("s3", "ap-southeast-1"))
    store = initKbtzStore kId (basePath)
    c = mkKbtzConf conf aws kId store sesh wp parConf ropts
  return (c, kNodes)

runHydration :: forall m. (HConM m, MonadSample m)
  => Time.UTCTime
  -> Http.WriteParams
  -> TKbtzim
  -> m ()
runHydration t0 wp kbtzim = -- S.after (liftIO . print $ "Hydration Finished!") $
  S.drain . S.fromWAsync $ S.mapM (h kbtzim) (configs kbtzim) 
  where
    h kns (c, kNodes) = hydrateKbtz t0 c kNodes $ unfoldNodes (life c) kns
    configs kns = S.mapM (mkKbtzConfM wp) $ S.unfold (unfoldKbtzim Finite kns) ()

ufStream :: forall t m a b. (HConS t m) => (a -> t m b) -> UF.Unfold m a b
ufStream f = UF.many (UF.function f) UF.fromStream

nodePrefixes :: forall m. (HConM m)
  => KbtzName
  -> (NodeMAC -> IO ())
  -> UF.Unfold m KbtzName NodeMAC
  -> UF.Unfold m NodeMAC Prefix
  -> S.WSerialT m (NodeMAC, Prefix)
nodePrefixes k nodeAction ns ps = do
  n' <- S.trace (liftIO . nodeAction) $ S.unfold ns k
  p <- S.unfold ps n'
  return $ (n', p)

hydrateKbtz :: forall m. (HConM m, MonadSample m)
  => Time.UTCTime
  -> KbtzConf
  -> TNodes
  -> UF.Unfold m KbtzName NodeMAC
  -> m ()
hydrateKbtz t0 KbtzConf{kbtzName, kbtzStore
                       , manOrSesh, res
                       , startTime, bucket
                       , env, writeParams
                       , life, parHow, ropts} tNodes ns = S.drain $ do
  (S.fromEffect . S.drain . S.fromAhead $ fp)
  `S.async`
  (S.fromEffect . S.drain . S.fromAhead $ kbtzState kbtzStore writeParams tNodes)
  where
    kp = S.map fst
        $ S.filter ((> 0) . snd)
       --  $ S.trace (pr . keyLog)
        S.|$ S.mapM (fetchKeys env bucket keyPath)
        S.|$ xs
    fp = S.map fst
        $ S.filter ((> 0) . snd)
        $ S.trace (pr . frameLog)
        S.|$ S.mapM (fetchFrames manOrSesh (bucket, env, t0) ropts (getKbtzPath kbtzStore))
        S.|$ kp
    xs = S.fromWSerial $ nodePrefixes kbtzName (mkNodeDirs kbtzStore) ns prefixes
    keyPath = getKbtzPath kbtzStore Keys
    prefixes = ufStream (prefixGen @S.SerialT life inSet res startTime t0)
    inSet :: NodeMAC -> m Bool
    inSet n = liftIO . atomically $ do
      s <- toMACSet <$> readTVar tNodes
      return $ M.member n s
    pr = liftIO . print
    prefLog (n, p) = "Prefix Generated :" <> show (n, p)
    keyLog ((n, p), i) = "Keys Downloaded" <> show (n, p, i)
    frameLog ((n, p), i) = "Frames Downloaded:" <> show (n, p, i)

prefixGen :: forall t m a. (HConS t m, Ord a)
  => LifeTime
  -> (a -> m Bool)
  -> (Resolution, Resolution)
  -> Time.UTCTime
  -> Time.UTCTime
  -> a
  -> t m Prefix
prefixGen life keep (pastRes, futureRes) start now n = case life of
  Finite -> past
  Infinite -> S.uniq (past <> future)
  where
    past = S.fromList (prefixRange pastRes start (Just now))
    {-# INLINE past #-}
    future = S.takeWhileM (\_ -> keep n) $ posthence futureRes
    {-# INLINE future #-}
{-# INLINE prefixGen #-}


deriving newtype instance W.Serialise ST.MilliSecond64

newtype S3Id = S3Id ST.MilliSecond64
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Bounded, Enum, Integral, Num, Real, W.Serialise)
  deriving newtype (Prim)

deriving newtype instance W.Serialise S3.ObjectKey

toS3Id :: S3.ObjectKey -> S3Id
toS3Id = S3Id . ST.MilliSecond64 . read . T.unpack . snd . T.breakOnEnd "/" . T.replace " " "" . unObject

newtype S3Idx a = S3Idx (S3Id, a)
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving (W.Serialise) via (W.WineryProduct (S3Idx a))

toS3Idx :: (S3Id, a) -> S3Idx a
toS3Idx = S3Idx

type S3Key = S3Idx S3.ObjectKey

type S3Req = S3Idx BS.ByteString

type S3Body = (S3Idx BS.ByteString)

type GetObjError = (S3Idx RespStatus)

data ResponseState = Normal | Throttling | Error GetObjError
  deriving (Eq, Ord, Show, Generic)
  deriving W.Serialise via (W.WineryVariant ResponseState)

newtype RespStatus = RespStatus (Int, BS.ByteString, ResponseState)
  deriving (Eq, Ord, Show, Generic)
  deriving W.Serialise via (W.WineryProduct RespStatus)

wrapStatus :: NC.Status -> RespStatus
wrapStatus s = RespStatus (NC.statusCode s, NC.statusMessage s, state)
  where
    state = case NC.statusCode s of
      503 -> Throttling
      _ -> Normal

type S3Resp = Either GetObjError S3Body

--(</>) :: FilePath -> FilePath -> FilePath
--a </> b = a <> "/" <> b

data StoreType = Keys | Frames | Errors | Ingested
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

storeTypeName :: StoreType -> FilePath
storeTypeName = T.unpack . T.toLower . showText

unStoreTypeName :: FilePath -> Maybe StoreType
unStoreTypeName "keys" = Just Keys 
unStoreTypeName "frames" = Just Frames
unStoreTypeName "errors" = Just Errors
unStoreTypeName "ingested" = Just Ingested
unStoreTypeName _ = Nothing

data KbtzStore = KbtzStore
  { mkNodeDirs :: NodeMAC -> IO ()
  , keyFolder :: NodeMAC -> FS.AbsDir
  , frameFolder :: NodeMAC -> FS.AbsDir
  , errFolder :: NodeMAC -> FS.AbsDir
  , ingestedFolder :: NodeMAC -> FS.AbsDir
  , keyFile :: NodeMAC -> Prefix -> Path Abs File
  , frameFile :: NodeMAC -> Prefix -> Path Abs File
  , errFile :: NodeMAC -> Prefix -> Path Abs File
  , ingestedFile :: NodeMAC -> Prefix -> Path Abs File
  , tagger :: NodeMAC -> KbtzNode
  } deriving (Generic)



getKbtzFolder :: KbtzStore -> StoreType -> (NodeMAC -> FS.AbsDir)
getKbtzFolder KbtzStore{..} = \case
  Keys -> keyFolder
  Frames -> frameFolder
  Errors -> errFolder
  Ingested -> ingestedFolder

getKbtzPath :: KbtzStore -> StoreType -> (NodeMAC -> Prefix -> Path Abs File)
getKbtzPath KbtzStore{..} = \case
  Keys -> keyFile
  Frames -> frameFile
  Errors -> errFile
  Ingested -> ingestedFile

data StoreEv = WriteStart StoreType NodeMAC Prefix (Path Abs File)
             | WriteEnd StoreType NodeMAC Prefix (Path Abs File)

getEvPath :: StoreEv -> Path Abs File
getEvPath (WriteStart _ _ _ p) = p
getEvPath (WriteEnd _ _ _ p) = p


toStoreEv :: (FS.MonadFS m) => Event -> m (Maybe StoreEv)
toStoreEv ev = case EvL.isWriteClosed ev of
  False -> return Nothing
  True -> do
    pth <- FS.evRelPath' ev
    abspth <- FS.evAbsPath' ev
    case s pth of
      Nothing -> return Nothing
      Just (Frames) -> return . Just $ WriteEnd Frames (n pth) (p pth) abspth
      _ -> return Nothing
  where
    p :: FS.RelFile -> Prefix
    p = toEnum . read . toFilePath . filename
    n = NodeId . T.pack . toFilePath . parent
    s = unStoreTypeName . toFilePath . parent . parent

class HasStoreType a where
  getStoreType :: StoreType

instance HasStoreType S3Key where
  getStoreType = Keys

instance HasStoreType S3Body where
  getStoreType = Frames

nodeA :: forall t m a. (HConS t m, W.Serialise a, HasStoreType a) => KbtzStore -> NodeMAC -> t m a
nodeA k n = S.concatMap x
  $ S.adapt
  $ fmap getEvPath
  $ watchNodeStore k (getStoreType @a) n 
  where
    x = S.map fromWino
      . S.rights
      . S.trace logEither
      . decodeFile @t @m @(Wino a)
      . toFilePath
  
watchNodeStore :: (HConM m) => KbtzStore -> StoreType -> NodeMAC -> S.SerialT m StoreEv
watchNodeStore k s n = S.concatM $ do
  d <- FS.arrFromPath (getKbtzFolder k s n)
  return $ S.catMaybes
    $ S.mapM toStoreEv
    $ S.hoist (liftIO)
    $ EvL.watchWith c [FS.unArrPath d]
  where
    c = EvL.setAttrsModified Off
      . EvL.setRootPathEvents Off
      . EvL.setRootMoved Off
      . EvL.setRootDeleted Off
      . EvL.setWhenExists EvL.ReplaceIfExists
      . EvL.setOnlyDir Off
      . EvL.setOneShot Off
      . EvL.setUnwatchMoved On
      . EvL.setFollowSymLinks Off
      . EvL.setRecursiveMode On


initKbtzStore :: KbtzName -> FS.AbsDir -> KbtzStore
initKbtzStore kbtz base = KbtzStore mkNodeDirs
    (hFolder Keys) (hFolder Frames) (hFolder Errors) (hFolder Ingested)
    (hFile Keys) (hFile Frames) (hFile Errors) (hFile Ingested) (asKbtzNode kbtz)
  where
    root = base </> (fromJust . FS.rootPath $ kbtz)
    nodeDirHere here n =  root </> here </> (frcRelDir $ nodeMACPath n)
    mkNodeDirHere p n = cd (toFilePath $ nodeDirHere p n)
    mkNodeDirs :: NodeMAC -> IO ()
    mkNodeDirs n = mapM_ (`mkNodeDirHere` n) storeDirNames
      where
        storeDirNames :: [FS.RelDir]
        storeDirNames = frcRelDir <$> storeTypeName <$>  [(minBound @StoreType)..maxBound]
    hFolder :: StoreType -> NodeMAC -> FS.AbsDir
    hFolder s n = base
                  </> (fromJust . FS.rootPath $ kbtz)
                  </> (frcRelDir . storeTypeName $ s)
                  </> (frcRelDir . nodeMACPath $ n)
    hFile :: StoreType -> NodeMAC -> Prefix -> Path Abs File
    hFile s n pref = hFolder s n
                     </> (fromJust . parseRelFile . T.unpack . asFileName $ pref)
    frcRelDir = fromJust . parseRelDir

kbtzState :: forall t m. (HConS t m, MonadSample m)
  => KbtzStore
  -> Http.WriteParams
  -> TNodes
  -> t m (NodeMAC, (SensorR, MeshR))
kbtzState store writeParams ns = S.concatM $ do
  hw <- toMACSet <$> (liftIO . atomically $ readTVar ns)
  return $ S.concatMapFoldableWith S.parallel nState $ M.toList hw
  where
    nState (n, h) = fmap (n,) $ nodeState (nodeFold n h) (fl n) (nodeA @t @m @S3Body store n)
    nodeFold :: NodeMAC -> HW Double -> NodeF m
    nodeFold n h = (FL.partition (sensorFold h) (meshFold n))
    fl n = FL.lmap (nodeLines) $ lineFoldHttp @m 32 writeParams
      where
        tag = tagger store n
        nodeLines = uncurry (<>) . bimap (lineSensorR tag) (lineMesh tag)



type NodeRF m = RF.Refold m NodeMAC (Either EnergyState RuntimeStats) (SensorR, MeshR)

type NodeF m = FL.Fold m (Either EnergyState RuntimeStats) (SensorR, MeshR)

rightsUF :: Monad m => UF.Unfold m x (Either e a) -> UF.Unfold m x a
rightsUF = UF.map (fromRight undefined) . UF.filter isRight

catMaybesUF :: Monad m => UF.Unfold m x (Maybe a) -> UF.Unfold m x a
catMaybesUF = UF.map fromJust . UF.filter isJust

nodeState :: forall t m. (HConS t m)
  => NodeF m
  -> FL.Fold m (SensorR, MeshR) ()  
  -> t m S3Body
  -> t m (SensorR, MeshR)
nodeState nodeFold sink x = S.tap (sink)
  S.|$ S.postscan nodeFold
  S.|$ S.catMaybes
  S.|$ S.trace logNothing
  S.|$ S.mapM (pure . validateMF')
  S.|$ S.rights
  S.|$ S.trace logEither
  S.|$ S.mapM (pure . parsePB)
  S.|$ x
  where
    parsePB = traverse (fmap fromPB . decodeA @(PB MeshFrame) . SBS.toArray)
    validateMF' :: S3Idx MeshFrame -> Maybe (Either EnergyState RuntimeStats)
    validateMF' (S3Idx (S3Id idx, m)) = case accessEnergyState m of
      (Just !e) -> Just . Left $ fixGridTS t e
      Nothing -> case accessRTS m of
        Just !r -> Just . Right $ fixMeshTS t r
        Nothing -> Nothing
     where
       t = Just . posixSecondsToUTCTime . fromIntegral $ div idx 1000

fetchFrames ::
  forall m. HConM m
  => Session.Session
  -> SignWith
  -> Options
  -> (StoreType -> NodeMAC -> Prefix -> Path Abs File)
  -> (NodeMAC, Prefix)
  -> m ((NodeMAC, Prefix), Int)
fetchFrames sesh sw ropts getPath (n, p) = fmap snd
  . S.fold (FL.partition errSink frameSink)
  . S.map (bimap toWino toWino)
  . S.tapRate 1 printDLRate
  . S.fromAhead . S.maxBuffer 3000 . S.mapM ((sessionS3 sw sesh ropts))
  $ keySource
  where
    printDLRate r = liftIO . print
      $ "Download rate from Node "
      <> show (n, p)
      <> " is " <> show r
    frameSink = saveWithLength (n, p) (encodeFold (toFilePath $ getPath Frames n p))
    errSink = encodeFold (toFilePath $ getPath Errors n p)
    -- dr = fmap (const ((n, p), 1)) $ Sink.toFold (Sink.drainM (\x -> (liftIO . print $ x)))
    -- dr' = Sink.toFold (Sink.drainM (liftIO . print))
    --  = Sink.drain (liftIO . print)
    keySource :: S.AheadT m S3Key
    keySource = decodeKeys readAll
      where
        readAll = decodeFile (toFilePath $ getPath Keys n p)
        decodeKeys = S.map fromWino
          . S.rights
          . S.trace logEither
{-# INLINE[1] fetchFrames #-}

saveWithLength :: (MonadIO m) => b -> FL.Fold m a () -> FL.Fold m a (b, Int)
saveWithLength tag f = FL.rmapM (\x -> return (tag, snd x))
                       (FL.tee f FL.length)
{-# INLINE saveWithLength #-}


type SignWith = (S3.BucketName, Env, Time.UTCTime)


sessionS3 :: forall m. (HConM m)
  => SignWith -> Session.Session -> Options -> S3Key -> m S3Resp
sessionS3 (S3.BucketName !bucket, _, _) !sesh !opts k = do
  -- liftIO $ print $ "Inside Session" <> (show k)
  !resp <- (onS3Idx s3GetSafe) $ k
  -- liftIO $ print $ "Response Received" <> (show resp)
  return $! a resp 
  where
    -- $ This exists because using traverse on S3Idx lead to all the compute being
    --  eaten and f not being applied. SO WEIRD. 
    onS3Idx :: (a -> m b) -> S3Idx a -> m (S3Idx b)
    onS3Idx f (!S3Idx (!x, !y)) = do
      !y' <- f y
      return $! S3Idx (x, y')
    a (!S3Idx (!idx, !o)) = bimap (S3Idx . (idx,)) (S3Idx . (idx,)) o
    s3GetSafe =
      recoverWith ("req" :: String) 1 (Left . wrapStatus $ NC.imATeapot418) . s3Get
    s3Get :: S3.ObjectKey -> m (Either RespStatus BS.ByteString)
    s3Get !r = liftIO $ do
      !r' <- checkResponse =<< (Session.getWith opts sesh . toReq $ r)
      return $! r'
    region = "ap-southeast-1"
    basePath = "https://" <> bucket <> ".s3." <> region <> ".amazonaws.com" <> "/"
    toReq (S3.ObjectKey !k) = T.unpack $ basePath <> k
    checkResponse :: NC.Response BL.ByteString -> IO (Either RespStatus BS.ByteString)
    checkResponse !resp = if NC.responseStatus resp == NC.ok200 then (do
                           -- liftIO . print $ "Checking Response"
                           let !bo = BL.toStrict (NC.responseBody resp)
                           -- liftIO . print $ "Checked Response"
                           return . Right $! bo)
                         else return . Left . wrapStatus $ NC.responseStatus resp
{-# INLINE[1] sessionS3 #-}



s3Paths'' :: forall m. (MonadIO m, MonadCatch m)
        => Env -> (Prefix -> S3.ListObjectsV2) -> UF.Unfold m Prefix (S3.ObjectKey, Int)
s3Paths'' env f = let
  plist = UF.map ((fmap (\a -> (a ^. S3.oKey, a ^. S3.oSize))) . (^. S3.lovrsContents))
    (UF.lmap f $ (pageUFM env))
  in UF.many plist UF.fromList
{-# INLINE s3Paths'' #-}


fetchKeys :: forall m. (HConM m)
  => Env
  -> S3.BucketName
  -> (NodeMAC -> Prefix -> Path Abs File)
  -> (NodeMAC, Prefix)
  -> m ((NodeMAC, Prefix), Int)
fetchKeys env bucket path (n, t) = do
  cached <- liftIO $ PIO.doesFileExist p
  case cached of
    True -> do
      l <- fromIntegral <$> (liftIO $ getFileSize $ toFilePath p)
      case l < 4 of
        True -> dl
        False -> return $ ((n, t), l)
    False -> dl
  where
    dl = UF.fold fodl (UF.map keyed (s3Paths'' env (req n))) t
    fodl =  saveWithLength (n, t) (FL.lmap toWino (encodeFold $ toFilePath p))
    p = path n t
    keyed = toS3Idx . (toS3Id &&& id) . fst
    req n' prefix = S3.listObjectsV2 bucket
      & S3.lovPrefix .~ timedPrefix n' prefix
      & S3.lovMaxKeys .~ (Just 1000000000)
          --  & S3.lovDelimiter .~ (Just '/')
{-# INLINE fetchKeys #-}

ourSettings :: ManagerSettings -> NC.ManagerSettings
ourSettings ManagerSettings{..} = cachingSettings
  where
    oneSec = 1000000
    cachingSettings = tlsManagerSettings
      { NC.managerConnCount = manConnCount
      , NC.managerIdleConnectionCount = manIdleConn
      , NC.managerResponseTimeout = NC.responseTimeoutMicro (manTimeout * oneSec)
      }



logNothing :: forall m a. (MonadIO m, Show a) => Maybe a -> m ()
logNothing x = when (isNothing x) $ liftIO $ print x
{-# INLINE logNothing #-}

logEither :: forall m a b. (MonadIO m, Show a) => Either a b -> m ()
logEither x = case x of
  Left a -> liftIO $ print a
  Right _ -> return ()
{-# INLINE logEither #-}
