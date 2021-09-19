{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeApplications, ScopedTypeVariables, ExplicitForAll, NamedFieldPuns, TupleSections, BangPatterns, OverloadedLists #-}
module Main where

import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId
import Chopaan.Comm.S3 hiding (pathFile)
import Chopaan.Types
import Streamly.Binary
import Chopaan.Kibbutz.AWS.Common hiding (preResolvingManager)
import Chopaan.Utils.Retry (recoverC, recoverOrNothing)
import Chopaan.Comm.Dispatch (accessEnergyState, accessRTS)
import Chopaan.Node.Folds (sensorFold, meshFold)
import Data.Influxable (asKbtzNode, lineSensorR, lineMesh, lineFoldHttp)


import Proto.NodeMessageSchema.NodeMessages (MeshFrame, EnergyState, RuntimeStats)
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N (cpuTime)

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Lens
import Data.Bifunctor
import Data.Word
import Data.Int
import Data.Maybe

import qualified Data.Text as T
import qualified Data.Time as Time
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

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.IsStream.Transform as S
import qualified Streamly.Internal.Data.Stream.IsStream.Common as S
import qualified Streamly.External.ByteString as SBS
import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Array.Foreign.Type as A
import qualified Streamly.Internal.Data.Array.Stream.Foreign as A
import Dhall


--import Paths_chopaan

getOpts = input auto $ "./hydration.dhall"
labNodes = NodeId <$> [ "8c:aa:b5:97:69:48"
                      , "ac:67:b2:11:f2:30"
                      , "8c:aa:b5:95:97:c8"
                      , "ac:67:b2:1c:ec:d8"
                      , "c4:4f:33:67:ea:69"
                      , "7c:9e:bd:f5:ec:74"
                      , "ac:67:b2:11:f0:28"
                      , "7c:9e:bd:f6:48:88"
                      ]


main :: IO ()
main = do
  (h :: HydrationOpts) <- getOpts
  t0 <- Time.getCurrentTime
  aws <- getAwsEnv S3.s3
  mapM_ (\n -> cd ("./data/paths/" <> (nodeMACPath n))) labNodes
  mapM_ (\n -> cd ("./data/signed-paths/" <> (nodeMACPath n))) labNodes
  mapM_ (\n -> cd ("./data/frames/" <> (nodeMACPath n))) labNodes
  let p = DB.queryParams defaultDatabase
  DB.manage p $ F.formatQuery ("CREATE DATABASE "F.%F.database) defaultDatabase
  --ps <- S.length . S.fromParallel
  --      $ S.tapRate 10 (\r -> print $ "Signing Rate: " <> (show r))
  --      $ signSavedPaths t0 h aws labNodes
        -- S.|$ signedPaths t0 h aws labNodes
  --_ <- downloadAllPrefixes t0 aws h labNodes
  ps <- S.length . S.fromAsync $ loadFrames (KbtzId "Lab_TestGrid") labNodes h
  t1 <- Time.getCurrentTime
  --print $ "Paths Signed: " <> (show ps)
  print $ "Start time: " <> (show t0)
  print $ "End time: " <> (show t1)
  let delT = Time.diffUTCTime t1 t0
  print $ "Total time taken: " <> (show delT)
  print $ "Time per Node: " <> (show $ delT / fromIntegral ps)


defaultDatabase = F.formatDatabase "InfluxDB"

pathFile :: NodeMAC -> Prefix -> FilePath
pathFile n (Prefix pref) = "./data/paths/" <> (nodeMACPath n) <> "/" <> (T.unpack pref) 

signedPathFile :: NodeMAC -> Prefix -> FilePath
signedPathFile n (Prefix pref) = "./data/signed-paths/" <> (nodeMACPath n) <> "/" <> (T.unpack pref)

frameFile :: NodeMAC -> Prefix -> FilePath
frameFile n (Prefix pref) = "./data/frames/" <> (nodeMACPath n) <> "/" <> (T.unpack pref)


downloadAllPrefixes :: forall m. (S.MonadAsync m, MonadCatch m, MonadThrow m)
  => Time.UTCTime -> Env -> HydrationOpts -> [NodeMAC] -> m ()
downloadAllPrefixes time env HydrationOpts{start, end, resolution, s3BucketName} ns =
  NC.withDNSCache cacheConf' $ \c -> do
    man <- cachingManager c
    S.drain $ S.fromSerial -- $ S.maxThreads 4
      $ S.mapM (uncurry (prefixDownload env time bucket c man)) $ nps  
  where
    nps = S.fromList $ (,) <$> ns <*> prefixes
    prefixes = prefixRange resolution (toUTC start) (toUTC end)
    bucket = S3.BucketName s3BucketName
    


loadPrefixFrames :: forall t m. (S.IsStream t, S.MonadAsync m, MonadCatch m)
  => NodeMAC -> Prefix -> t m (MeshFrame)
loadPrefixFrames n prefix = S.map (fromPB)
  $ S.rights
  $ S.trace (liftIO . printLeft)
  $ S.map (decodeA @(PB MeshFrame))
  $ S.rights
  $ S.tapRate 1 (\r -> liftIO . print $ "ReadRate: " <> (show n)
                       <> "_" <> (show prefix) <> ": " <> (show r))
  $ S.trace (liftIO . printLeft)
  $ (decodeFile @t @m @(A.Array Word8) (frameFile n prefix))


validateMF' :: MeshFrame -> Maybe (Either EnergyState RuntimeStats)
validateMF' m = case accessEnergyState m of
                  Just e -> Just $ Left e -- (fixGridTS e)
                  Nothing ->
                    case accessRTS m of
                      Just ((r :: RuntimeStats)) -> Just $ Right r -- (fixMeshTS r)
                      Nothing -> Nothing
  where
    fixGridTS :: EnergyState -> Maybe EnergyState
    fixGridTS r = case r ^? N.cpuTime of
      Nothing -> Nothing
      (Just t') -> Just r
    {-# INLINE fixGridTS #-}
    fixMeshTS :: RuntimeStats -> Maybe RuntimeStats
    fixMeshTS r = case r ^? N.cpuTime of
      Nothing -> Nothing
      (Just t') -> Just r
    {-# INLINE fixMeshTS #-}



loadFrames :: (S.IsStream t, S.MonadAsync m, MonadCatch m)
  => KbtzName -> [NodeMAC] -> HydrationOpts -> t m ()
loadFrames k ns HydrationOpts{resolution, start, end} =
  S.mapM (processNode fl prefixes k) (S.fromList ns)
  where
    fl = lineFoldHttp 32 (Http.writeParams defaultDatabase)
    prefixes = prefixRange resolution (toUTC start) (toUTC end)

processNode :: forall m. (S.MonadAsync m, MonadCatch m)
  => FL.Fold m [Line Time.UTCTime] () ->  [Prefix] -> KbtzName -> NodeMAC -> m ()
processNode lnFold prefixes k n = S.fold lnFold
  $ S.fromAhead
  $ S.maxThreads 3000
  $ S.tapRate 10 (\r -> liftIO $ print $ "Ingest Rate for " <> (show n) <> " : " <> (show r))
  $ S.map (\(a, b) -> a <> b)
  $ S.map (bimap (lineSensorR tag) (lineMesh tag))
  $ S.postscan (FL.partition sensorFold (meshFold n))
  --  $ S.map fromJust
  --  $ S.filter isJust
  $ S.catMaybes
  $ S.trace (\x -> if (isNothing x) then (liftIO $ print x) else (return ()))
  $ S.map (validateMF')
  $ S.concatMapWith S.ahead (\p -> loadPrefixFrames n p) $ S.fromList prefixes
  where
    tag = asKbtzNode k n



prefixDownload :: forall m. (S.MonadAsync m, MonadCatch m, MonadThrow m)
  => Env
  -> Time.UTCTime
  -> S3.BucketName
  -> NC.DNSCache
  -> NC.Manager
  -> NodeMAC
  -> Prefix
  -> m ()
prefixDownload env time bucket cache man n p = S.fold (saveDL n p)
  $ A.compact A.defaultChunkSize
  $ S.rights
  $ S.catMaybes
  $ S.trace (printLeft)
  $ S.tapRate 10 (\r -> liftIO $ print $ "Download rate from Node "
                        <> (show n)
                        <> " And Chunk: "
                        <> (show p) <> " is " <> (show r))
  $ S.maxThreads 3000
  $ S.fromAhead
  $ S.mapM ((liftIO . mkReq) <=< toReq)
  $ S.maxThreads 1000
  $ S.mapM (signPath bucket env time)
  $ loadPrefixPaths n p 
  where
    mkReq req = recoverOrNothing "req" 3 ((NC.withResponse req man checkResponseEtc))
      where
        checkResponseEtc :: NC.Response NC.BodyReader -> IO (Either NC.Status (A.Array Word8)) 
        checkResponseEtc resp = case (NC.responseStatus resp == NC.ok200) of
          True -> do
            bo <- BS.concat <$> (NC.brConsume . NC.responseBody $ resp)
            return . Right . SBS.toArray $ bo
          _ -> return . Left $ (NC.responseStatus resp)
    printLeft x = case x of
      Nothing -> liftIO $ print "error!"
      Just x' -> case x' of
        Left a -> liftIO $ print a
        Right _ -> return ()
    toReq :: BS.ByteString -> m NC.Request
    toReq bs = do
      r <- NC.parseRequest . T.unpack . T.decodeUtf8 $ bs
      -- ha <- liftIO $ NC.lookup cache (NC.host r)
      return $ r -- { NC.hostAddress = ha }
    -- host = "s3-ap-southeast-1.amazonaws.com"
    saveDL n t = (encodeFold (frameFile n t))


printLeft x = case x of
  Left a -> liftIO $ print a
  Right _ -> return ()

signedPaths :: forall m. (S.MonadAsync m, MonadCatch m)
  => Time.UTCTime -> HydrationOpts -> Env -> [NodeMAC] -> S.ParallelT m (BS.ByteString)
signedPaths startTime HydrationOpts{resolution, start, end, s3BucketName, bufOpts} env ns =
  S.concatMapWith S.parallel nodePaths $ S.fromList ns
  where
    prefixes = S.fromList $ prefixRange resolution (toUTC start) (toUTC end)
    savePrefix n t = (FL.lmap (toWino . (first unObject)) (encodeFold (pathFile n t)))
    nodePaths :: NodeMAC -> S.ParallelT m (BS.ByteString)
    nodePaths n = S.concatMapWith S.parallel (S.fromAhead . prefixPaths n) prefixes 
    prefixPaths :: NodeMAC -> Prefix -> S.AheadT m (BS.ByteString)
    prefixPaths n t = S.tap (saveSigned n t)
        $ S.mapM (signPath bucket env startTime)
        $ S.tap (savePrefix n t)
        $ S.maxBuffer 0
        $ S.unfold (s3Paths'' env (req n)) t
    req n (Prefix t) = S3.listObjectsV2 bucket
          & S3.lovPrefix .~ (timedPrefix n t)
    saveSigned n t = (FL.lmap (toWino) (encodeFold (signedPathFile n t)))
    bucket = S3.BucketName s3BucketName

    
signPath :: (MonadIO m) => S3.BucketName -> Env -> Time.UTCTime -> (S3.ObjectKey, Int) -> m (BS.ByteString)
signPath bucket env = sign 
    where
      sign t w = liftIO $ withAwsEnv env (liftAWS . presignURL t (oneHour) . toReq $ w)
      readObjReq k = S3.getObject bucket k
      toReq = readObjReq . fst
      oneMin = 60
      oneHour = 60 * oneMin


--downloadPrefixFrames :: MonadIO m => NodeMAC -> Prefix -> m ()
--downloadPrefixFrames n p = S.mapM () S.mapM (signPath bucket env time) $ loadPrefixPaths n p


loadSignedPaths :: forall t m. (S.IsStream t, S.MonadAsync m, MonadCatch m)
                => NodeMAC -> Prefix -> t m (BS.ByteString)
loadSignedPaths n prefix = S.map fromWino
                           $ S.rights
                           $ (decodeFile @t @m @(Wino (BS.ByteString)) (signedPathFile n prefix))

loadPrefixPaths :: forall t m. (S.IsStream t, S.MonadAsync m, MonadCatch m)
                => NodeMAC -> Prefix -> t m (S3.ObjectKey, Int)
loadPrefixPaths n prefix = S.map ((first S3.ObjectKey) . fromWino)
      $ S.rights
      $ (decodeFile @t @m @(Wino (T.Text, Int)) (pathFile n prefix))

signSavedPaths :: forall t m. (S.IsStream t, S.MonadAsync m, MonadCatch m)
  => Time.UTCTime -> HydrationOpts -> Env -> [NodeMAC] -> t m (Bool)
signSavedPaths time HydrationOpts{resolution, start, end, s3BucketName} env ns =
  S.concatMapWith S.parallel (signer) pathFiles
  where
    prefixes = prefixRange resolution (toUTC start) (toUTC end)
    signer (n, prefix) = S.map (const True)
      $ S.tap (saveSigned n prefix)
      $ S.mapM (signPath bucket env time)
      $ loadPrefixPaths n prefix
    pathFiles = S.fromList $ (,) <$> ns <*> prefixes
    bucket = (S3.BucketName s3BucketName)
    saveSigned n t = (FL.lmap (toWino) (encodeFold (signedPathFile n t)))


preResolvingManager :: forall m. (S.MonadAsync m) => m NC.Manager
preResolvingManager = NC.withDNSCache cacheConf' cachingManager


cachingManager :: (MonadIO m) => NC.DNSCache -> m NC.Manager
cachingManager c = liftIO $ NC.newManager cachingSettings
  where
    cachingSettings = tlsManagerSettings
      { NC.managerConnCount = 512
      , NC.managerIdleConnectionCount = 512
      , NC.managerResponseTimeout = NC.responseTimeoutMicro (60 * oneSec)
      -- , NC.managerModifyRequest = preResolveReq c  
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
  , NC.maxConcurrency = 512
  , NC.minTTL = 10
  , NC.maxTTL = 30
  , NC.negativeTTL = 300
  }
