{-# LANGUAGE RankNTypes, TypeApplications, ExplicitForAll, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Chopaan.Utils.StreamsInterop where

import Streamly
import qualified Streamly.Prelude as S
import qualified Reflex as R

import Control.Monad (void, liftM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Streamly.Internal.Data.Stream.StreamK (hoist)


-- | streamly to event
toEvent :: forall t t' m m'. (IsStream t, MonadAsync m, R.Reflex t', R.TriggerEvent t' m', MonadIO m')
        => (forall a. m a -> IO a) -> (forall a. Show a => t m a -> m' (R.Event t' a))
toEvent h s = do
  (e, fire) <- R.newTriggerEvent
  liftIO . S.drain $ S.mapM (void . fire) $ (hoist h) . adapt $ s
  return e
