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

import Data.Pool (stats)

import System.Remote.Monitoring (forkServer)

import Network.AWS.S3 (BucketName(..))
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S

import Options.Applicative
import RIO hiding (view, async, withAsync, Async)
import qualified Data.Time as Ti


type KbtzM = ReaderT (MQTTOpts) GraphM

runKbtzim :: forall t.
  (S.IsStream t)
  => MQTTOpts
  -> HydrationOpts
  -> GraphM (t GraphM Bool)
runKbtzim mq hydrationOpts = do
  tNow <- liftIO $ Ti.getCurrentTime
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
  let confss = S.fromList $ fmap sConf (zip ks nss)-- qss
      past = S.concatMapWith S.async
        (hydrateKbtz' hydrationOpts) confss
      --present = S.concatMapWith S.async (S.concatM . runKibbutz @t) confss
  return $ (S.map (const True) $ past)
    --`S.parallel` (S.map (const True) $ present)
  where
    futPrefix = "runKibbutz :"
    labKbtz = (KbtzId "Lab_TestGrid")
    -- labNodes = NodeId <$> [ "7c:9e:bd:f5:ec:74", "c4:4f:33:67:ea:69"
    --                           , "ac:67:b2:11:e5:c4", "7c:9e:bd:f6:43:88" ]
    labNodes = NodeId <$> [ "8c:aa:b5:97:69:48"
                          , "ac:67:b2:11:f2:30"
                          , "8c:aa:b5:95:97:c8"
                          , "ac:67:b2:1c:ec:d8"
                          , "c4:4f:33:67:ea:69"
                          , "7c:9e:bd:f5:ec:74"
                          , "ac:67:b2:11:f0:28"
                          , "7c:9e:bd:f6:48:88"
                          ]
    t0 = toUTC (start hydrationOpts)
    tn = toUTC (end hydrationOpts)
    sConf (k, ns) = KbtzC { Chopaan.Kibbutz.name = k
                          , nodes = ns
                          , channelOpts = Left mq
                          , s3Opts = Just (BucketName (s3BucketName hydrationOpts))
                          }


run :: RIO App ()
run = do
  hSetBuffering stdout LineBuffering 
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  tc <- liftIO $ execParser tkOptions
  liftIO $ forkServer "localhost" 8111
  liftIO $ runGraphM poolConf tc $ do
    sp <- spools <$> ask
    ks <- S.tapRate 30 (\_ -> liftIO $ monitorSpool sp)
      <$> (runKbtzim @S.AheadT mqttOpts hydrationOpts)
    S.drain $ S.fromAhead ks
