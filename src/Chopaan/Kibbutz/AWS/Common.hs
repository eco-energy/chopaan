{-# LANGUAGE ScopedTypeVariables, CPP, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts, OverloadedStrings #-}
module Chopaan.Kibbutz.AWS.Common
  ( inAwsContext
  , pageUF
  , pageUFM
  , pageS
  , newLogger
  , LogLevel (..)
  , AWSC
  , Logger
  , withAwsEnv
  , getAwsEnv
  , Env(..)
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Resource.Internal
import Control.Monad.Trans.Control
import Network.AWS.Env
import Network.HTTP.Client.Internal (hostAddress)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.DNS.Resolver
import qualified Network.DNS.Cache as NC

import Data.Maybe
import qualified Data.Time as Time
import Lens.Micro
import System.IO (stdout, withFile, IOMode(..), openFile)
import System.Environment


import qualified Streamly.Data.Unfold as UF
import qualified Streamly.Prelude as S
import Chopaan.Utils.Retry (recoverC)

type AWSC b = AWST' Env (ResourceT IO) b

creds :: FilePath -> Credentials
creds fp = FromFile "default" fp --"/run/keys/aws-creds"
  --FromEnv "AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY" Nothing (Just "ap-southeast-1")

inAwsContext :: Logger -> Service -> AWSC b -> IO b
inAwsContext lgr svc ma = do
  env <- getAwsEnv svc
  runResourceT . runAWST env $ ma

withAwsEnv :: Env -> AWSC b -> IO b
withAwsEnv env ma = runResourceT . runAWST env $ ma

frmrl :: Credentials
frmrl = (FromProfile "chopaanRole")

getAwsEnv :: (S.MonadAsync m, MonadCatch m) => Service -> m Env
getAwsEnv svc = do
  liftIO . print $ "AWS ENV REQUESTED!"
  e <- liftIO $ lookupEnv "AWS_CREDS"
  t <- liftIO $ (Time.formatTime Time.defaultTimeLocale "%y-%m-%d-%R-%Q") <$> Time.getCurrentTime
  let fname = "aws_log_" <> t --(showText . toText . _svcAbbrev $ svc) <> "_" <> t
  lgHandle <- liftIO $ openFile fname WriteMode
  lgr <- newLogger Info lgHandle
  manager <- preResolvingManager
  case e of
    Nothing -> error "AWS CONTEXT NOT AVAILABLE, AWS_CREDS NOT DEFINED"
    Just fp -> newEnvWith (creds fp) Nothing manager
      <&> set envLogger lgr . set envRegion Singapore
      <&> set envRetryCheck (retryConnectionFailure 50)
      <&> configure svc

preResolvingManager :: forall m. (S.MonadAsync m) => m Manager
preResolvingManager = NC.withDNSCache cacheConf cachingManager
  where
    cacheConf :: NC.DNSCacheConf
    cacheConf = NC.DNSCacheConf
      { NC.resolvConfs = [
          defaultResolvConf { resolvInfo = RCHostNames ["8.8.8.8","8.8.4.4"]
                            --, resolvConcurrent = True
                            }]
      , NC.maxConcurrency = 1000
      , NC.minTTL = 60
      , NC.maxTTL = 300
      , NC.negativeTTL = 300
      }
    cachingManager :: NC.DNSCache -> m Manager
    cachingManager c = liftIO $ newManager cachingSettings
      where
        cachingSettings = tlsManagerSettings
          { managerConnCount = 100
          , managerModifyRequest = preResolveReq c  
          }
        preResolveReq cache r = do
          h <- liftIO $ NC.lookup cache (host r)
          _ <- if isNothing h
                 then liftIO . print $ "COULD NOT RESOLVE HOST: " <> (show h)
                 else return ()
          let r' = r { hostAddress = h }
          return r'

pageUF :: forall m a r. (AWSPager a, AWSConstraint r m) => UF.Unfold m a (Rs a)
pageUF = UF.lmap Just $ UF.unfoldrM step
  where
    step :: (Maybe a) -> m (Maybe (Rs a, Maybe a))
    step Nothing = return Nothing
    step (Just req) = do
      y <- send req
      return $ Just (y, page req y)


pageUFM :: forall m a. (MonadIO m, MonadCatch m, AWSPager a) => Env -> UF.Unfold m a (Rs a)
pageUFM env = UF.lmap Just $ UF.unfoldrM step
  where
    step :: (Maybe a) -> m (Maybe (Rs a, Maybe a)) 
    step Nothing = return Nothing
    step (Just req) = do
      y <- liftIO $ withAwsEnv env
           $ recoverC ("paging retry" :: String) 10 $ timeout 120 $ send req
      return $ Just (y, page req y)

pageS :: forall t m a. (S.IsStream t, S.MonadAsync m, MonadCatch m, AWSPager a) => Env -> a -> t m (Rs a)
pageS env req = S.unfoldrM step start
  where
    start = Just req
    step :: (Maybe a) -> m (Maybe (Rs a, Maybe a)) 
    step Nothing = return Nothing
    step (Just req') = do
      y <- liftIO $ withAwsEnv env $ recoverC ("paging retry" :: String) 10 $ timeout 120 $ send req'
      return $ Just (y, page req' y)



instance MonadBase b m => MonadBase b (ResourceT m) where
    liftBase = lift . liftBase

instance MonadTransControl ResourceT where
#if MIN_VERSION_monad_control(1,0,0)
    type StT ResourceT a = a
    liftWith f = ResourceT $ \r -> f $ \(ResourceT t) -> t r
    restoreT = ResourceT . const
#else
    newtype StT ResourceT a = StReader {unStReader :: a}
    liftWith f = ResourceT $ \r -> f $ \(ResourceT t) -> liftM StReader $ t r
    restoreT = ResourceT . const . liftM unStReader
#endif
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (ResourceT m) where
#if MIN_VERSION_monad_control(1,0,0)
     type StM (ResourceT m) a = StM m a
     liftBaseWith f = ResourceT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ runInBase . (\(ResourceT r) -> r reader'  )
     restoreM = ResourceT . const . restoreM
#else
     newtype StM (ResourceT m) a = StMT (StM m a)
     liftBaseWith f = ResourceT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ liftM StMT . runInBase . (\(ResourceT r) -> r reader'  )
     restoreM (StMT base) = ResourceT $ const $ restoreM base
#endif
