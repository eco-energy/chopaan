{-# LANGUAGE RankNTypes #-}

module Chopaan.Utils.StreamsInterop where

import Streamly
import qualified Streamly.Prelude as S
import qualified Reflex as R
import Reflex.Vty (VtyWidget)

import Control.Monad (void, liftM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Concurrent (forkIO)

{--
-- | reflex to streamly
fromEvent :: (IsStream t, Monad m) => m (R.Event t' a) -> t m a
fromEvent = S.unfoldrM unconsE
    where
    unconsE v = do
        e <- v
        return $ Just (e, v)
--}

-- | streamly to event
toEvent :: (MonadIO m, R.TriggerEvent t' m') => SerialT m a -> (VtyWidget t' m' (R.Event t' a))
toEvent s = do
  (e, fire) <- R.newTriggerEvent
  pure $ S.mapM_ (liftIO . void . fire) s
  return e
  

--main = do
--    S.toList (fromVector (V.fromList [1..3]))   >>= print
--    V.toList (toVector (S.fromFoldable [1..3])) >>= print
