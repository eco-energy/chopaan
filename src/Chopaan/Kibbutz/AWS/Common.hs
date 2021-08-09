{-# LANGUAGE ScopedTypeVariables #-}
module Chopaan.Kibbutz.AWS.Common
  ( inAwsContext
  , pageUF
  , newLogger
  , LogLevel (..)
  , AWSC
  , Logger
  , withAwsEnv
  , getAwsEnv
  ) where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Lens.Micro
import System.IO (stdout)

import qualified Streamly.Data.Unfold as UF

type AWSC b = AWST' Env (ResourceT IO) b

inAwsContext :: Logger -> Service -> AWSC b -> IO b
inAwsContext lgr svc ma = do
  env <- newEnv Discover <&> set envLogger lgr . set envRegion Singapore <&> configure svc  
  runResourceT . runAWST env $ ma

withAwsEnv :: Env -> AWSC b -> IO b
withAwsEnv env ma = runResourceT . runAWST env $ ma

getAwsEnv :: (MonadIO m, MonadCatch m) => Service -> m Env
getAwsEnv svc = do
  lgr <- newLogger Info stdout
  env <- newEnv Discover <&> set envLogger lgr . set envRegion Singapore <&> configure svc
  return env
  
pageUF :: forall m a r. (AWSPager a, AWSConstraint r m) => UF.Unfold m a (Rs a)
pageUF = UF.lmap Just $ UF.unfoldrM step
  where
    step :: (Maybe a) -> m (Maybe (Rs a, Maybe a)) 
    step Nothing = return Nothing
    step (Just req) = do
      y <- send req
      return $ Just (y, page req y)

