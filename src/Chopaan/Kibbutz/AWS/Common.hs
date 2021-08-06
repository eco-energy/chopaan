{-# LANGUAGE ScopedTypeVariables #-}
module Chopaan.Kibbutz.AWS.Common
  ( inAwsContext
  , pageUF
  , newLogger
  , LogLevel (..)
  , AWSC
  , Logger
  ) where

import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Lens.Micro

import qualified Streamly.Data.Unfold as UF

type AWSC b = AWST' Env (ResourceT IO) b

inAwsContext :: Logger -> Service -> AWSC b -> IO b
inAwsContext lgr svc ma = do
  env <- newEnv Discover <&> set envLogger lgr . set envRegion Singapore <&> configure svc  
  runResourceT . runAWST env $ ma

pageUF :: forall m a r. (AWSPager a, AWSConstraint r m) => UF.Unfold m a (Rs a)
pageUF = UF.lmap Just $ UF.unfoldrM step
  where
    step :: (Maybe a) -> m (Maybe (Rs a, Maybe a)) 
    step Nothing = return Nothing
    step (Just req) = do
      y <- send req
      return $ Just (y, page req y)

