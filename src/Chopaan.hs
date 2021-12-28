{-# LANGUAGE TypeApplications, ScopedTypeVariables, RecordWildCards, FlexibleContexts, OverloadedStrings #-}
module Chopaan where

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Chopaan.Kibbutz
import Chopaan.Hydrate ( hConfDef, mkTKbtz, runHydration )
import Chopaan.Kibbutz.KbtzId ( KbtzId(KbtzId) )
import Chopaan.Node.NodeId ( NodeId(NodeId), NodeMAC )
import Chopaan.API.History ()
import Chopaan.Types
    ( App(appOptions),
      Options(Options, influxConn, poolConf, hydrationOpts, dbOpts,
              kibbutzOpts, nodeOpts, mqttOpts, logVerbose),
      HydrationOpts(s3BucketName),
      KibbutzOpts(KibbutzOpts, name),
      MQTTOpts,
      InfluxConn,
      icOptions )
import Chopaan.Graph.Kbtz ( getKbtzim )
import Chopaan.Graph
    ( GraphM, runGraphM, withKbtzPool, tkOptions, getKNs, addzim )
import Data.Influxable (createDB)
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Pool (stats)
import qualified Data.Map.Strict as M

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.FileSystem.Handle as H
import qualified Streamly.Internal.FileSystem.File as File


import System.Remote.Monitoring (forkServer)

import Network.AWS.S3 (BucketName(..))
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S

import Options.Applicative ( execParser )
import RIO
    ( MonadIO(liftIO),
      stdout,
      ReaderT,
      MonadReader(ask),
      BufferMode(LineBuffering),
      RIO,
      hSetBuffering,
      atomically )
import qualified Data.Time as Ti


type KbtzM = ReaderT (MQTTOpts) GraphM


runKbtzim :: forall t.
  (S.IsStream t)
  => MQTTOpts
  -> HydrationOpts
  -> InfluxConn
  -> GraphM (t GraphM Bool)
runKbtzim mq hydrationOpts influxCon = do
  tNow <- liftIO $ Ti.getCurrentTime
  ks' <- withKbtzPool getKbtzim
  kns <- case (length ks' < 1) of
    True -> do
      liftIO . print $ "Adding " <> (show (fst deployKbtz))
      addzim [deployKbtz]
    False -> getKNs
  kbtzim <- liftIO . atomically $ mkTKbtz kns
  let confss = S.fromList $ fmap sConf $ fmap undefined $ M.toList kns
      s3Hydration = S.fromEffect ((pure . (const True)) =<< (runHydration influxCon hConfDef kbtzim))
      mqttStream = S.map (const True)
        $ S.concatMapWith S.parallel (S.concatM . runKibbutz @t) confss
  return $ s3Hydration `S.parallel` mqttStream
  where 
    deployKbtz = (KbtzId "Bismillah_Mor", fmap fst deployNodes)
    sConf (k, ns) = KbtzC { Chopaan.Kibbutz.name = k
                          , structure = ns
                          , channelOpts = Left mq
                          , s3Opts = Just (BucketName (s3BucketName hydrationOpts))
                          , influxCon = influxCon
                          }

type NodeKey = NodeId Int

newtype DispatchNodes m n x = DispatchNodes (UF.Unfold m n x) 

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
  ic <- liftIO $ execParser icOptions
  liftIO $ forkServer "localhost" 8111
  liftIO $ createDB influxConn "chopaanMQTT"
  liftIO $ runGraphM poolConf tc $
    S.drain . S.fromAhead =<< (runKbtzim @S.AheadT mqttOpts hydrationOpts ic)
    
