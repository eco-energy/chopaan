{-# LANGUAGE TypeApplications, ScopedTypeVariables, RecordWildCards, FlexibleContexts #-}
module Chopaan where

import Control.Monad.IO.Class
import Chopaan.Kibbutz
import Chopaan.API.History
import Chopaan.Types
import Chopaan.Graph.Kbtz

import Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S

import Options.Applicative
import RIO hiding (view, async, withAsync, Async)


runKbtzim :: forall t m.
  (IsStream t, MonadAsync m)
  => TinkerConf
  -> MQTTOpts
  -> m (t m Bool)
runKbtzim (TinkerConf h p) mq = (toHandlerH h p) . (fmap S.adapt)
                                . (fmap (S.hoist (toHandlerH h p) . S.serially)) $  do
  ks <- withKbtzPool getKbtzim
  nss <- mapM (\k -> withKbtzPool (flip getKbtzNodes k)) ks
  qss <- mapM (\(k, ns) -> Left <$> mqttQs mq k ns) $ zip ks nss
  let confss = fmap sConf $ zip (zip ks nss) qss
  return
    $ S.concatMapM runKibbutz
    $ S.fromList confss
  where
    sConf ((k, ns), qs) = KbtzC { Chopaan.Kibbutz.name = k
                                , nodes = ns
                                , channelOpts = qs
                                , spiderHost = h
                                , spiderPort = p
                                }


run :: RIO App ()
run = do
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  tkOpts <- liftIO $ execParser tkOptions
  ks <- liftIO $ runKbtzim @ParallelT tkOpts mqttOpts
  liftIO . S.drain $ S.adapt ks
