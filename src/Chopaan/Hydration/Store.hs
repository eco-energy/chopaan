{-# LANGUAGE FlexibleContexts #-}
module Chopaan.Hydration.Store where

import Control.Monad.IO.Class

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.FileSystem.Event.Linux as Ev
import Data.Word

unfoldWithWait :: (S.MonadAsync m) => UF.Unfold m FilePath Word8
unfoldWithWait = undefined

-- $ Based on the cache used by cas-store



-- cacheKleisliIO :: MonadIO m =>
--   Maybe Int
--   -> CacherM m i o
--   -> ContentStore
--   -> remoteCache
--   -> (i -> m o)
--   -> i
--   -> m o
-- cacheKleisliIO = undefined
