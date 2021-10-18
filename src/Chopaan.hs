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
import Data.Influxable (createDB)
import Data.Bifunctor
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
  kns <- case (length ks' < 1) of
    True -> do
      liftIO . print $ "Adding " <> (show (fst deployKbtz))
      addzim [deployKbtz]
    False -> getKNs
  let confss = S.fromList $ fmap sConf kns
      -- past = S.concatMapWith S.wAsync
      --   (hydrateKbtz' hydrationOpts) confss
      present = S.concatMapWith S.wAsync (S.concatM . runKibbutz @t) confss
  return $ S.mapM (pure . const True) $ present
  where
    deployKbtz = (KbtzId "", fmap fst deployNodes)
    sConf (k, ns) = KbtzC { Chopaan.Kibbutz.name = k
                          , nodes = ns
                          , channelOpts = Left mq
                          , s3Opts = Just (BucketName (s3BucketName hydrationOpts))
                          }

type NodeKey = NodeId Int

deployNodes :: [(NodeMAC, NodeKey)]
deployNodes = (bimap NodeId NodeId) <$>
  [ ("7c:9e:bd:48:4e:e0",  1)
  , ("7c:9e:bd:f5:ec:74",  3)
  , ("ac:67:b2:11:f3:10",  5)
  , ("7c:9e:bd:49:07:68",  6)
  , ("ac:67:b2:11:f2:30",  8)
  , ("8c:aa:b5:97:69:48",  9)
  , ("8c:aa:b5:95:8f:9c", 10)
  , ("ac:67:b2:1c:ec:d8", 11)
  , ("7c:9e:bd:47:8a:5c", 12)
  , ("7c:9e:bd:49:1d:80", 13)
  , ("ac:67:b2:1d:e7:f4", 14)
  , ("7c:9e:bd:47:b7:e8", 15)
  ]

labNodes = NodeId <$> [ "ac:67:b2:11:f3:20",
                        "ac:67:b2:1d:e7:f4",
                        "8c:aa:b5:97:69:48",
                        "8c:aa:b5:95:97:c8",
                        "8c:aa:b5:95:8f:9c",
                        "ac:67:b2:1c:ec:d8",
                        "7c:9e:bd:f5:ec:74",
                        "ac:67:b2:11:f0:28"
                      ]
labNodes1 :: [NodeMAC]
labNodes1 = NodeId <$>
  [ "ac:67:b2:11:f3:10"
  , "ac:67:b2:12:07:b0"
  , "7c:9e:bd:47:61:bc"
  , "7c:9e:bd:47:b7:e8"
  , "7c:9e:bd:48:4e:e0"
  , "7c:9e:bd:48:a2:c4"
  , "ac:67:b2:11:e6:e4"
  ]

run :: RIO App ()
run = do
  hSetBuffering stdout LineBuffering 
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  tc <- liftIO $ execParser tkOptions
  liftIO $ forkServer "localhost" 8111
  liftIO $ createDB "chopaanMQTT"
  liftIO $ runGraphM poolConf tc $ do
    sp <- spools <$> ask
    ks <- (runKbtzim @S.AheadT mqttOpts hydrationOpts)
    S.drain $ S.fromAhead ks
