{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeApplications, ScopedTypeVariables, ExplicitForAll, NamedFieldPuns, TupleSections, BangPatterns, OverloadedLists, PolyKinds, DataKinds, UnboxedTuples #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable, DerivingVia, CPP, LambdaCase, RecordWildCards, RankNTypes #-}
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
import Data.Influxable (KbtzNode, asKbtzNode, lineSensorR, lineMesh, lineFoldHttp, showText)


import Proto.NodeMessageSchema.NodeMessages (MeshFrame, EnergyState, RuntimeStats)
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Lens
import Data.Bifunctor
import Data.Word
import Data.Int
import Data.Maybe
import Data.Either

import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time.Clock.POSIX.Compat (posixSecondsToUTCTime)

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import Network.AWS
import qualified Network.AWS.S3 as S3

import Network.DNS.Resolver
import qualified Network.DNS.Cache as NC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types as NC
import qualified Network.HTTP.Client as NC
import qualified Network.HTTP.Client.Internal as NC (hostAddress)

import Database.InfluxDB.Line (Line)
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
import qualified Streamly.Internal.Data.Stream.IsStream.Transform as S
import qualified Streamly.Internal.Data.Stream.IsStream.Common as S
import qualified Streamly.External.ByteString as SBS
import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Array.Foreign.Type as A
import qualified Streamly.Internal.Data.Array.Stream.Foreign as A
import qualified Streamly.Internal.Data.Time.Units as ST
import Streamly.Internal.Data.IORef.Prim (Prim(..))
import Dhall
import System.Directory

#if 0
import Paths_chopaan
#else
getDataFileName = pure 
#endif

getOpts = do
  path <- getDataFileName "hydration.dhall"
  input auto $ T.pack path
  
labNodes = NodeId <$> [ "8c:aa:b5:97:69:48"
                      , "ac:67:b2:11:f2:30"
                      , "8c:aa:b5:95:97:c8"
                      , "ac:67:b2:1c:ec:d8"
                      , "c4:4f:33:67:ea:69"
                      , "7c:9e:bd:f5:ec:74"
                      , "ac:67:b2:11:f0:28"
                      , "7c:9e:bd:f6:48:88"
                      ]

data DLConf = DLConf
  { env :: Env
  , bucket :: S3.BucketName
  , nodes :: [NodeMAC]
  , prefixes :: [Prefix]
  , kbtzStore :: KbtzStore
  } deriving (Generic)


main :: IO ()
main = do
  (HydrationOpts{s3BucketName, start, end, resolution}) <- getOpts
  t0 <- Time.getCurrentTime
  let prefixes = prefixRange resolution (toUTC start) (toUTC end)
      bucket = S3.BucketName s3BucketName
  aws <- getAwsEnv S3.s3
  let p = DB.queryParams chopaanDB
  DB.manage p $ F.formatQuery ("CREATE DATABASE "F.%F.database) chopaanDB
  hydrateKbtz basePath aws chopaanDB kbtzId labNodes
  t1 <- Time.getCurrentTime
  print $ "Start time: " <> (show t0)
  print $ "End time: " <> (show t1)
  let delT = Time.diffUTCTime t1 t0
  print $ "Total time taken: " <> (show delT)
  --print $ "Time per Node: " <> (show $ delT / fromIntegral ps)
  where
    kbtzId = KbtzId "Lab_TestGrid"
    writeParams = Http.writeParams chopaanDB
    chopaanDB :: DB.Database
    chopaanDB = F.formatDatabase "chopaan"

hydrateKbtz :: FilePath -> Env -> DB.Database -> KbtzName -> [NodeMAC]
hydrateKbtz basePath aws chopaanDB kbtzId ns = do
  t0 <- Time.getCurrentTime
  store <- initKbtzStore basePath kbtzId ns
  flip (runReaderT (DLConf aws bucket nodes prefixes kbtzStore)) $ do
    _ <- getKeysFS
    print $ "Keys downloaded"
    td <- Time.getCurrentTime
    let delTd = Time.diffUTCTime td t0
    print $ "Key download time: " <> (show delTd)
    _ <- kbtzFramesFS
    loadFrames store (Http.writeParams chopaanDB) kbtzId nodes


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

data StoreType = Keys | Frames | Errors
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

storeTypeName :: StoreType -> FilePath
storeTypeName = T.unpack . T.toLower . showText

data KbtzStore = KbtzStore
  { keyFile :: (NodeMAC -> Prefix -> FilePath)
  , frameFile :: (NodeMAC -> Prefix -> FilePath)
  , errFile :: (NodeMAC -> Prefix -> FilePath)
  } deriving (Generic)

getKbtzPath :: KbtzStore -> StoreType -> (NodeMAC -> Prefix -> FilePath)
getKbtzPath KbtzStore{..} = \case
  Keys -> keyFile
  Frames -> frameFile
  Errors -> errFile

initKbtzStore :: (MonadIO m) => FilePath -> KbtzName -> [NodeMAC] -> m (KbtzStore)
initKbtzStore base kbtz ns = do
  mkNodesDirsHeres (storeTypeName <$> [(minBound @StoreType)..maxBound]) ns
  return $ KbtzStore (hFile Keys) (hFile Frames) (hFile Errors)
  where
    root = base </> (T.unpack . showText $ kbtz)
    nodeDirHere here n =  root </> here </> (nodeMACPath n)
    mkNodeDirHere p n = liftIO $ cd (nodeDirHere p n)
    mkNodesDirsHeres heres ns = mapM_ (uncurry mkNodeDirHere) ((,) <$> heres <*> ns)
    hFile :: StoreType -> NodeMAC -> Prefix -> FilePath
    hFile s n (Prefix pref) = base
                              </> (storeTypeName s)
                              </> (T.unpack (unKbtzId kbtz))
                              </> (nodeMACPath n)
                              </> (T.unpack pref)


loadFrames :: (S.MonadAsync m, MonadCatch m)
  => KbtzStore -> Http.WriteParams -> KbtzName -> [NodeMAC] -> m ()
loadFrames store writeParams k ns =
  S.drain $ S.fromAsync
  $ S.mapM (ingestNodeFramesFS store tag fl) (S.fromList ns)
  where
    fl = lineFoldHttp 32 writeParams
    tag = asKbtzNode k

ingestNodeFramesFS :: forall m. (S.MonadAsync m, MonadCatch m)
  => KbtzStore -> (NodeMAC -> KbtzNode) -> FL.Fold m [Line Time.UTCTime] () -> NodeMAC -> m ()  
ingestNodeFramesFS store lnFold getTag n = ingestFrames uf getTag lnFold n 
  where
    uf = (UF.many (UF.function framePath) File.read)
    framePath n = getKbtzPath store Frames n (Prefix "all")

ingestFrames :: forall m. (S.MonadAsync m, MonadCatch m)
  => UF.Unfold m NodeMAC Word8
  -> FL.Fold m [Line Time.UTCTime] ()
  -> (NodeMAC -> KbtzNode)
  -> NodeMAC
  -> m ()
ingestFrames source lnFold tag n = do
  S.fold lnFold
    $ S.fromAhead
    $ S.maxThreads 3000
    $ S.tapRate 10 (\r -> liftIO $ print $ "Ingest Rate for " <> (show n) <> " : " <> (show r))
    $ S.postscan (nodeFold)
    $ S.catMaybes
    $ S.trace (logNothing)
    $ S.map (validateMF')
    $ S.rights
    $ S.trace (logEither)
    $ S.map parsePB
    $ decodeFrames
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



kbtzFramesFS :: forall m. (S.MonadAsync m, MonadCatch m, MonadThrow m)
  => ReaderT DLConf m ()
kbtzFramesFS = do
  DLConf{..} <- ask
  lift . liftIO $ NC.withDNSCache cacheConf' $ \c -> do
    time <- Time.getCurrentTime
    let sw = (bucket, env, time)
    man <- cachingManager c
    S.drain
      $ S.fromAsync . (S.maxThreads (length nodes))
      $ S.mapM (downloadNodeFS kbtzStore man sw prefixes) $ S.fromList nodes
{-# INLINE kbtzFramesFS #-}

downloadNodeFS :: forall m. (S.MonadAsync m, MonadCatch m, MonadThrow m)
  => KbtzStore -> NC.Manager -> SignWith ->  [Prefix] -> NodeMAC -> m () 
downloadNodeFS store man sw ps n = downloadFS man sw prefixKeys frameSink errSink n
  where
    frameSink = (getKbtzPath store Frames n (Prefix "frames"))
    errSink = (getKbtzPath store Errors n (Prefix "errors"))
    prefixKeys = (getKbtzPath store Keys n) <$> ps
{-# INLINE downloadNodeFS #-}

downloadFS :: forall m a. (S.MonadAsync m, MonadCatch m, MonadThrow m, Show a)
  => NC.Manager -> SignWith -> [FilePath] -> FilePath -> FilePath -> a -> m ()
downloadFS man sw sourceKeys sinkFrame sinkErr tag =
  download man sw File.read (S.fromList sourceKeys) (encodeFold sinkFrame) (encodeFold sinkErr) tag
{-# INLINE downloadFS #-}

download :: forall m a tag. (S.MonadAsync m, MonadCatch m, MonadThrow m, Show tag)
  => NC.Manager -> SignWith
  -> UF.Unfold m a Word8 -- ^ Should be decodable to a stream of S3Keys
  -> (forall t. S.IsStream t => t m a)
  -> FL.Fold m (Wino S3Body) ()
  -> FL.Fold m (Wino GetObjError) ()
  -> tag
  -> m ()
download man signWith sourceUF sourceSeeds saveDL saveErr tag = (fmap snd)
  . S.fold (FL.partition saveErr saveDL)
  . S.fromAhead . S.maxThreads 1
  . S.map (bimap toWino toWino)
  . S.tapRate 10 (printDLRate)
  . (dl man signWith)
  . decodeKeys
  $ S.unfoldMany sourceUF sourceSeeds
  where
    printDLRate r = liftIO . print
      $ "Download rate from Node "
      <> (show tag)
      <> " is " <> (show r)
{-# INLINE download #-}


dl :: forall m. (S.MonadAsync m, MonadCatch m, MonadThrow m)
   => NC.Manager -> SignWith -> S.AheadT m S3Key -> S.AheadT m S3Resp
dl man signWith = S.trace (logEither)
                  . S.maxThreads 1000
                  . S.mapM (goIdxd)
                  . S.mapM (signIdxd)
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

getKeysFS :: ReaderT DLConf m ()
getKeysFS = do
  store <- kbtzStore <$> ask
  getKeys (toFile store)
  where
    toFile :: KbtzStore -> NodeMAC -> Prefix -> FL.Fold m S3Key () 
    toFile store n t = FL.lmap toWino (encodeFold (getKbtzPath store Keys n t))
{-# INLINE getKeysFS #-}

getKeys :: forall m. (S.MonadAsync m, MonadCatch m)
  => (NodeMAC -> Prefix -> FL.Fold m S3Key ())
  -> ReaderT DLConf m ()
getKeys prefixFold = do
  DLConf{env, bucket, nodes, prefixes} <- ask
  let
    prefixKeys :: NodeMAC -> Prefix -> m ()
    prefixKeys n t = (UF.fold
                      (prefixFold n t)
                      (UF.map (toS3Idx . (toS3Id &&& id) . fst) (s3Paths'' env (req n)))) t
      where
        req n (Prefix t) = S3.listObjectsV2 bucket
          & S3.lovPrefix .~ (timedPrefix n t)
          
  lift $ S.drain . S.fromParallel . S.maxThreads 10000
    $ S.mapM (uncurry prefixKeys)
    $ S.fromList ((,) <$> nodes <*> prefixes)
{-# INLINE getKeys #-}

decodeKeys :: forall t m. (S.IsStream t, S.MonadAsync m, MonadCatch m)
                => t m Word8 -> t m S3Key
decodeKeys = (S.map fromWino) . S.rights . (S.trace logEither) . decodeS 
{-# INLINE decodeKeys #-}

loadKeyFile :: forall t m. (S.IsStream t, S.MonadAsync m, MonadCatch m)
                => FilePath -> t m S3Key
loadKeyFile path = S.map (fromWino)
      $ S.rights
      $ decodeFile path

cachingManager :: (MonadIO m) => NC.DNSCache -> m NC.Manager
cachingManager c = liftIO $ NC.newManager cachingSettings
  where
    cachingSettings = tlsManagerSettings
      { NC.managerConnCount = 1024
      , NC.managerIdleConnectionCount = 512
      , NC.managerResponseTimeout = NC.responseTimeoutMicro (90 * oneSec)
      , NC.managerModifyRequest = preResolveReq c  
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
      ]-- { -- resolvInfo = RCHostNames ["8.8.8.8","8.8.4.4"]
                        -- resolvConcurrent = True
                        -- , resolvCache = False
                        --}]
  , NC.maxConcurrency = 10000
  , NC.minTTL = 10
  , NC.maxTTL = 30
  , NC.negativeTTL = 300
  }

logNothing :: forall m a. (MonadIO m) => Maybe a -> m ()
logNothing = \x -> if (isNothing x) then (liftIO $ print x) else (return ())

logEither :: forall m a b. (MonadIO m, Show a) => Either a b -> m ()
logEither x = case x of
  Left a -> liftIO $ print a
  Right _ -> return ()
