{-# LANGUAGE FlexibleContexts, BangPatterns, RankNTypes #-}
module Chopaan.Utils.Streamly where

import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class
import Data.Either
import Data.Maybe
import Streamly
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
  pure (fmap (fromRight undefined) $ S.filter isRight $ (Left <$> writes) `async` (Right <$> reads1), reads2)


sampleOn
  :: MonadAsync m
  => IsStream t
  => t m a
  -> t m (a -> b)
  -> t m b
sampleOn src pulse =
  S.mapMaybe id $
    S.scan fld combined
  where
  combined =
    runTillEndOfEitherWith
      parallel (Left <$> src) (Right <$> pulse)
  fld = FL.Fold step begin done
  -- First is the latest value of source,
  -- second is the value which to be yield'ed
  step _ (Left !a) = pure (Just a, Nothing)
  step (!x, _) (Right !f) = pure $ (x, f <$> x)
  begin = pure (Nothing, Nothing)
  done (_, out) = pure out


{-# INLINE runTillEndOfEitherWith #-}
runTillEndOfEitherWith
  :: forall t m a
  . IsStream t
  => Monad m
  => (forall c. t m c -> t m c -> t m c)
  -> t m a
  -> t m a
  -> t m a
runTillEndOfEitherWith combine src1 src2 =
  S.mapMaybe id $
    S.takeWhile isJust $
      ((Just <$> src1) `serial` S.yield Nothing)
        `combine`
      ((Just <$> src2) `serial` S.yield Nothing)
