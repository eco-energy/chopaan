module Chopaan.Utils.Retry where

import Control.Retry
import Control.Exception
import Control.Monad.Catch
import Data.Either
import Control.Monad.IO.Class

chopaanPolicy :: (MonadIO m) => RetryPolicyM m
chopaanPolicy = exponentialBackoff 50 <> limitRetries 10

recoverC :: (MonadIO m, MonadMask m) => m a -> m a
recoverC action = recoverAll chopaanPolicy (\_ -> action)

retryEither :: (MonadIO m) => n -> (n -> m (Either a b)) -> m (Either a b)
retryEither n f = retrying chopaanPolicy shouldRetryEither (\retryStatus ->  (liftIO $ print retryStatus) >> f n)

shouldRetryEither :: (Monad m) => RetryStatus -> (Either a b) -> m Bool
shouldRetryEither _ = return . isLeft

retryBool :: (MonadIO m) => m (Bool) -> m (Bool)
retryBool action = retrying retryPolicyDefault (\_ a -> return $ a == True) (\retryStatus ->  do
                                                                                x <- action
                                                                                return x
                                                                            )
