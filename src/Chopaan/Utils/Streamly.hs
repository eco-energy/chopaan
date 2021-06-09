{-# LANGUAGE FlexibleContexts, BangPatterns, RankNTypes #-}
module Chopaan.Utils.Streamly where

import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class
import Data.Either
import Data.Maybe

import Streamly.Prelude
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL


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
  pure (fmap (fromRight undefined) $ S.filter isRight $ (Left <$> writes) `S.async` (Right <$> reads1), reads2)


sampleOn
  :: S.MonadAsync m
  => S.IsStream t
  => t m a
  -> t m (a -> b)
  -> t m b
sampleOn src pulse =
  S.mapMaybe id $
    S.scan fld combined
  where
  combined =
    runTillEndOfEitherWith
      S.parallel (Left <$> src) (Right <$> pulse)
  fld = FL.Fold step begin done
  -- First is the latest value of source,
  -- second is the value which to be yield'ed
  step _ (Left !a) = pure . FL.Partial $ (Just a, Nothing)
  step (!x, _) (Right !f) = pure . FL.Partial $ (x, f <$> x)
  begin = pure (Nothing, Nothing)
  done (_, out) = pure out


{-# INLINE runTillEndOfEitherWith #-}
runTillEndOfEitherWith
  :: forall t m a
  . S.IsStream t
  => Monad m
  => (forall c. t m c -> t m c -> t m c)
  -> t m a
  -> t m a
  -> t m a
runTillEndOfEitherWith combine src1 src2 =
  S.mapMaybe id $
    S.takeWhile isJust $
      ((Just <$> src1) `S.serial` S.yield Nothing)
        `combine`
      ((Just <$> src2) `S.serial` S.yield Nothing)
