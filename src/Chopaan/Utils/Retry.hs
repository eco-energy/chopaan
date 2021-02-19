module Chopaan.Utils.Retry where

import Control.Retry
import Control.Exception
import Control.Monad.Catch
import Data.Either
import Control.Monad.IO.Class

chopaanPolicy :: (MonadIO m) => Int -> RetryPolicyM m
chopaanPolicy n = exponentialBackoff 10 <> limitRetries n

recoverC :: (MonadIO m, MonadMask m) => Int -> m a -> m a
recoverC n action = recoverAll (chopaanPolicy n) (\_ -> (liftIO $ print "retrying") >> action)

retryEither :: (MonadIO m) => n -> (n -> m (Either a b)) -> m (Either a b)
retryEither n f = retrying (chopaanPolicy 10) shouldRetryEither (\retryStatus ->  (liftIO $ print retryStatus) >> f n)

shouldRetryEither :: (Monad m) => RetryStatus -> (Either a b) -> m Bool
shouldRetryEither _ = return . isLeft

retryBool :: (MonadIO m) => m (Bool) -> m (Bool)
retryBool action = retrying retryPolicyDefault (\_ a -> return $ a == True) (\retryStatus ->  do
                                                                                x <- action
                                                                                return x
                                                                            )
