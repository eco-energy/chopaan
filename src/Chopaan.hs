{-# LANGUAGE TypeApplications, ScopedTypeVariables, RecordWildCards, FlexibleContexts, OverloadedStrings #-}
module Chopaan where

import Control.Monad.IO.Class
import Chopaan.Kibbutz
import Chopaan.Hydrate
import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.NodeId
import Chopaan.API.History
import Chopaan.Types
import Chopaan.Graph.Kbtz
import Chopaan.Graph

import Network.AWS.S3 (BucketName(..))
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S

import Options.Applicative
import RIO hiding (view, async, withAsync, Async)
import qualified Data.Time as Ti

runKbtzim :: forall t.
  (S.IsStream t)
  => MQTTOpts
  -> GraphM (t GraphM Bool)
runKbtzim mq = do
  ks' <- withKbtzPool getKbtzim
  ks <- case length ks' of
    0 -> do
      liftIO . print $ "Adding Lab Kbtz"
      withKbtzPool (flip addKbtz labKbtz)
      withKbtzPool (\c -> mapM_ (addNodeToKbtz c labKbtz) labNodes)
      withKbtzPool getKbtzim
    _ -> do
      liftIO . print $ "Kibbutzim: " <> (show ks')
      return ks'
  nss <- mapM (\k -> withKbtzPool (flip getKbtzNodes k)) ks
  --qss <- mapM (\(k, ns) -> mqttQs mq k ns) $ zip ks nss
  let confss = fmap sConf (zip ks nss)-- qss
      past = S.concatMapWith S.parallel (S.concatM . (flip hydrateKbtz $ (t0, tn))) $ S.fromList confss
      present = S.concatMapWith S.parallel (S.concatM . runKibbutz @t) $ S.fromList confss
  return $ (S.map (const True) $ past) `S.parallel`
    (S.map (const True) $ present)
  where
    futPrefix = "runKibbutz :" 
    labKbtz = (KbtzId "Lab_TestGrid")
    labNodes = NodeId <$> [ "7c:9e:bd:f5:ec:74", "c4:4f:33:67:ea:69"
                              , "ac:67:b2:11:e5:c4", "7c:9e:bd:f6:43:88" ]
    t0 = Ti.UTCTime (Ti.fromGregorian 2021 8 20) (Ti.secondsToDiffTime 0)
    tn = Ti.UTCTime (Ti.fromGregorian 2021 8 27) (Ti.secondsToDiffTime 0)
    sConf (k, ns) = KbtzC { Chopaan.Kibbutz.name = k
                          , nodes = ns
                          , channelOpts = Left mq
                          , s3Opts = Just (BucketName "dosti-datastream")
                          }


run :: RIO App ()
run = do
  hSetBuffering stdout LineBuffering 
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  TinkerConf{..} <- liftIO $ execParser tkOptions
  liftIO $ runGraphM (janusHost) (janusPort) $ do
    ks <- runKbtzim @S.AsyncT mqttOpts
    S.drain $ S.fromAsync ks
