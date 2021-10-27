{-# LANGUAGE RankNTypes, TypeApplications, ExplicitForAll, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs, ConstraintKinds #-}
module Chopaan.Utils.StreamsInterop where

import Streamly
import qualified Streamly.Prelude as S
{--
import qualified Reflex as R

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Streamly.Internal.Data.Stream.StreamK (hoist)
import Control.Concurrent (forkIO)


type StoRConstraints t t' m m' = (IsStream t, MonadAsync m, R.Reflex t', R.TriggerEvent t' m', MonadIO m')

type RtoSConstraints t t' m = (IsStream t, MonadAsync m, R.Reflex t', R.MonadHold t' m)

-- | streamly to event
toEvent :: forall t t' m m'. (StoRConstraints t t' m m')
        => (forall a. m a -> IO a) -> (forall a. Show a => t m a -> m' (R.Event t' a))
toEvent h s = do
  (e, fire) <- R.newTriggerEvent
  _ <- liftIO . forkIO . S.drain $ S.mapM (void . fire) $ (hoist h) . adapt $ s
  return e

toDynamic ::  forall t t' m m'. (R.MonadHold t' m', StoRConstraints t t' m m')
        => (forall a. m a -> IO a) -> (forall a. (Monoid a, Show a) => t m a -> m' (R.Dynamic t' a))
toDynamic h s = R.holdDyn mempty =<< (toEvent h s)


fromEvent :: forall t t' m. (RtoSConstraints t t' m) => (forall a. R.Event t' a -> t m a) 
fromEvent e = S.unfoldrM iterOverEvent e
  where
    iterOverEvent :: R.Event t' a -> m (Maybe (a, R.Event t' a))
    iterOverEvent ex = do
      b <- R.hold Nothing $ Just <$> ex
      h <- R.sample b
      case h of
        Nothing -> return $ Nothing
        Just he -> return $ Just (he, ex)

inIO :: forall t m a. (IsStream t, Monad m) => (forall b. m b -> IO b) -> t m a -> t IO a
inIO f s = hoist f s
--}
