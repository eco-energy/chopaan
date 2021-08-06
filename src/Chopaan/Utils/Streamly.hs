{-# LANGUAGE FlexibleContexts, BangPatterns, RankNTypes #-}
module Chopaan.Utils.Streamly where

import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class
import Data.Either
--import Data.Maybe
import Streamly.Prelude
import qualified Streamly.Prelude as S


duplicateS
  :: MonadAsync m
  => IsStream t
  => t m a
  -> m (t m a, t m a)
duplicateS src = do
  (writeChan', readChan1, readChan2) <- liftIO $ do
    chan <- TChan.newBroadcastTChanIO
    chan' <- STM.atomically $ TChan.dupTChan chan
    chan'' <- STM.atomically $ TChan.dupTChan chan
    pure (chan, chan', chan'')
  let
    writes =
      S.mapM (liftIO . STM.atomically . TChan.writeTChan writeChan') src
    reads1 =
      S.repeatM (liftIO $ STM.atomically $ TChan.readTChan readChan1)
    reads2 =
      S.repeatM (liftIO $ STM.atomically $ TChan.readTChan readChan2)
  pure (fmap (fromRight undefined) $ S.filter isRight $ (Left <$> writes) `async` (Right <$> reads1), reads2)
