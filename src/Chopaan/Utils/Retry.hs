{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Chopaan.Utils.Retry where

import Control.Retry
import qualified Control.Exception as E
import qualified Control.Monad.Catch as C
import Data.Either
import Control.Monad.IO.Class

chopaanPolicy :: (MonadIO m) => Int -> RetryPolicyM m
chopaanPolicy n = exponentialBackoff 10 <> limitRetries n

recoverC :: (MonadIO m, C.MonadMask m, Show e) => e -> Int -> m a -> m a
recoverC msg n action = recovering (chopaanPolicy n) [logDef] (\_ -> action)
  where
    logDef r = logRetries (\_ -> return True) (\b (C.SomeException e) rr -> liftIO $ print $ defaultLogMsg b e rr) r

recoverOrNothing :: forall m a e. (MonadIO m, C.MonadMask m, C.MonadCatch m, Show e) => e -> Int -> m a -> m (Maybe a)
recoverOrNothing msg n act = C.catchAll ((pure . Just) =<< (recoverC msg n act)) (\e -> (liftIO . print $ ("Failed After Retries: " <> show e))  >> return Nothing) 

recoverWith :: forall m a e. (MonadIO m, C.MonadMask m, C.MonadCatch m, Show e) => e -> Int -> a -> m a -> m a
recoverWith msg n c act = C.catchAll (recoverC msg n act) (\e -> (liftIO . print $ ("Failed After Retries: " <> show e))  >> (return c)) 

retryEither :: (MonadIO m) => n -> (n -> m (Either a b)) -> m (Either a b)
retryEither n f = retrying (chopaanPolicy 10) shouldRetryEither (\retryStatus ->  (liftIO $ print retryStatus) >> f n)

shouldRetryEither :: (Monad m) => RetryStatus -> (Either a b) -> m Bool
shouldRetryEither _ = return . isLeft

retryBool :: (MonadIO m) => m (Bool) -> m (Bool)
retryBool action = retrying retryPolicyDefault (\_ a -> return $ a == True) (\retryStatus ->  do
                                                                                x <- action
                                                                                return x
                                                                            )
--logDef :: (MonadIO m, C.MonadMask m, C.Exception e) => RetryStatus -> C.Handler m Bool
