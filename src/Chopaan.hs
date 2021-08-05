{-# LANGUAGE TypeApplications, ScopedTypeVariables, RecordWildCards, FlexibleContexts, OverloadedStrings #-}
module Chopaan where

import Control.Monad.IO.Class
import Chopaan.Kibbutz
import Chopaan.API.History
import Chopaan.Types
import Chopaan.Graph.Kbtz
import Chopaan.Graph

import Network.AWS.S3 (BucketName(..))
import Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S

import Options.Applicative
import RIO hiding (view, async, withAsync, Async)


runKbtzim :: forall t.
  (IsStream t)
  => MQTTOpts
  -> GraphM (t GraphM Bool)
runKbtzim mq = do
  ks <- withKbtzPool getKbtzim
  nss <- mapM (\k -> withKbtzPool (flip getKbtzNodes k)) ks
  qss <- mapM (\(k, ns) -> mqttQs mq k ns) $ zip ks nss
  let confss = fmap sConf $ zip (zip ks nss) qss
      past = S.concatMapWith S.parallel (S.concatM . hydrateKbtz @t) $ S.fromList confss
      present = S.concatMapWith S.parallel (S.concatM . runKibbutz @t) $ S.fromList confss
  return $ (S.map (const True) $ past) `parallel` (S.map (const True) $ present)
  where
    sConf ((k, ns), qs) = KbtzC { Chopaan.Kibbutz.name = k
                                , nodes = ns
                                , channelOpts = qs
                                , s3Opts = Just (BucketName "dosti-datastream")
                                }


run :: RIO App ()
run = do
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  TinkerConf{..} <- liftIO $ execParser tkOptions
  liftIO $ runGraphM (janusHost) (janusPort) $ do
    ks <- runKbtzim @ParallelT mqttOpts
    S.drain $ S.parallely ks
