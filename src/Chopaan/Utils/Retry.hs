module Chopaan.Utils.Retry where

import Control.Retry
import Data.Either
import Control.Monad.IO.Class

retryEither :: (MonadIO m) => n -> (n -> m (Either a b)) -> m (Either a b)
retryEither n f = retrying retryPolicyDefault shouldRetryEither (\retryStatus ->  f n)

shouldRetryEither :: (Monad m) => RetryStatus -> (Either a b) -> m Bool
shouldRetryEither _ = return . isLeft

retryBool :: (MonadIO m) => m (Bool) -> m (Bool)
retryBool action = retrying retryPolicyDefault (\_ a -> return $ a == True) (\retryStatus ->  do
                                                                                x <- action
                                                                                return x
                                                                            )
