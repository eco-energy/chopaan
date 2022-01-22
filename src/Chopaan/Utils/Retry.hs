{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Chopaan.Utils.Retry where

import Control.Retry
import qualified Control.Exception as E
import qualified Control.Monad.Catch as C
import Data.Either
import Control.Monad.IO.Class
import Streamly.Internal.Data.SVar (ThreadAbort(..))
import Network.HTTP.Client (HttpException(..))

chopaanPolicy :: (MonadIO m) => Int -> RetryPolicyM m
chopaanPolicy n = fullJitterBackoff 100000 <> limitRetries n

skipThreadAbort :: Monad m => RetryStatus -> C.Handler m Bool  
skipThreadAbort = \_ -> C.Handler $ \ (_ :: ThreadAbort) -> return False

logMsg :: (MonadIO m, C.Exception e) => String -> Bool -> e -> RetryStatus -> m ()
logMsg msg b e rr = liftIO $ print $ (show msg) <> (defaultLogMsg b e rr)

retryHttp :: MonadIO m => String -> RetryStatus -> C.Handler m Bool
retryHttp msg r = logRetries (\(a :: HttpException) -> case a of
                                 HttpExceptionRequest _ _ -> return True
                                 InvalidUrlException _ _ -> return False
                             ) logHttp r
  where
    logHttp b (HttpExceptionRequest r c) retry = case b of
      True -> liftIO . print $ "retrying, attempt: "
        <> (show $ rsIterNumber retry) <> (show c)
      False -> liftIO . print $ "crashed, attempt: "
        <> (show $ rsIterNumber retry) <> (show r) <> "\n\n" <> (show c)
    logHttp _ (InvalidUrlException url reason) _ = liftIO . print
      $ "INVALID URL, CHECK YOU CODE!" <> "\n\n" <> (show url) <> "\n\n" <> (show reason)  

catchThese msg r = flip C.catches [retryHttp msg r] 

recoverC :: (MonadIO m, C.MonadMask m, Show e) => e -> Int -> m a -> m a
recoverC msg n action = recovering (chopaanPolicy n) (skipAsyncExceptions <> [retryHttp (show msg)
                                                                             , skipThreadAbort
                                                                             , logDef]) (\_ -> action)
  where
    logDef r = logRetries (\_ -> return True) (\b (C.SomeException e) rr ->
                                                 liftIO $ print $
                                                 (show msg) <> (defaultLogMsg b e rr)) r

recoverOrNothing :: forall m a e. (MonadIO m, C.MonadMask m, C.MonadCatch m, Show e) => e -> Int -> m a -> m (Maybe a)
recoverOrNothing msg n act = C.catchAll ((pure . Just) =<< (recoverC msg n act)) (\e -> (liftIO . print $ ("Failed After Retries: " <> show e))  >> return Nothing) 

recoverWith :: forall m a e. (MonadIO m, C.MonadMask m, C.MonadCatch m, Show e)
  => e -> Int -> a -> m a -> m a
recoverWith msg n c act =  recovering (chopaanPolicy n)
  (skipAsyncExceptions <> [retryHttp (show msg)
                          , skipThreadAbort
                          , logDef]) (\r -> case (rsIterNumber r <= n) of
                                         True -> act
                                         False -> pure c
                                     )
  where
    logDef r = logRetries (\_ -> return True) (\b (C.SomeException e) rr ->
                                                 liftIO $ print $
                                                 (show msg) <> (defaultLogMsg b e rr)) r

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
