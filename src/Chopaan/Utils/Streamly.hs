{-# LANGUAGE FlexibleContexts, BangPatterns, RankNTypes #-}
module Chopaan.Utils.Streamly where

import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class
import Data.Either
import Data.Maybe
--import Data.Maybe
import Streamly.Prelude
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL

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


idFold :: (Monad m) => FL.Fold m a a
idFold = fmap fromJust $ FL.foldl' (flip (const . Just)) Nothing
{-# INLINE idFold #-}

secondF :: (Monad m) => FL.Fold m b c -> FL.Fold m (a, b) (a, c)
secondF = FL.unzip idFold
{-# INLINE secondF #-}

firstF :: (Monad m) => FL.Fold m a b -> FL.Fold m (a, c) (b, c)
firstF = (flip FL.unzip) idFold 
{-# INLINE firstF #-}

dupF :: (Monad m) => FL.Fold m a b -> FL.Fold m a (a, b)
dupF f = FL.tee idFold f  
{-# INLINE dupF #-}
