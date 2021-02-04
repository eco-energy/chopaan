{-# LANGUAGE ScopedTypeVariables #-}
module Chopaan.Kibbutz.AWS.Common where

import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Lens.Micro

import qualified Streamly.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold.Types as UF
import qualified Streamly.Internal.Data.Stream.StreamD.Type as STy


inAwsContext :: Logger -> Service -> AWST' Env (ResourceT IO) b -> IO b
inAwsContext lgr svc ma = do
  env <- newEnv Discover <&> set envLogger lgr . set envRegion Singapore <&> configure svc  
  runResourceT . runAWST env $ ma

pageUF :: forall m a r. (AWSPager a, AWSConstraint r m) => UF.Unfold m a (Rs a)
pageUF = UF.Unfold step inject
  where
    step :: Maybe a -> m (STy.Step (Maybe a) (Rs a)) 
    step (Just req) = do
      y <- send req
      return $ STy.Yield y (page req y)
    step Nothing = do
      return $ STy.Stop
    inject :: a -> m (Maybe a)
    inject = pure . Just
