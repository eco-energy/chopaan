module Chopaan.Utils.Retry where

import Control.Retry
import Data.Either

retryEither :: n -> (n -> IO (Either a b)) -> IO (Either a b)
retryEither n f = retrying retryPolicyDefault shouldRetryEither (\retryStatus ->  f n)

shouldRetryEither :: (Monad m) => RetryStatus -> (Either a b) -> m Bool
shouldRetryEither _ = return . isLeft

