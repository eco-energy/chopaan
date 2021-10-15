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

getKNs = withKbtzPool $ \c -> do
  ks' <- getKbtzim c
  nss <- mapM (\k -> withKbtzPool (flip getKbtzNodes k)) ks'
  return $ zip ks' nss
  
addzim :: [(KbtzName, [NodeMAC])] -> GraphM ([(KbtzName, [NodeMAC])]) 
addzim kns = withKbtzPool $ \c -> do
  mapM_ (addKbtz c) (fst <$> kns)
  sequence_ $ an c
  getKNs
  where
    an c = mconcat $ fmap (\(k, ns) -> (addNodeToKbtz c k) <$> ns) kns

runKbtzim :: forall t.
  (S.IsStream t)
  => MQTTOpts
  -> HydrationOpts
  -> GraphM (t GraphM Bool)
runKbtzim mq hydrationOpts = do
  tNow <- liftIO $ Ti.getCurrentTime
  ks' <- withKbtzPool getKbtzim
  kns <- case (length ks' < 2) of
    True -> do
      liftIO . print $ "Adding Lab Kbtz 1"
      addzim kns
    False -> getKNs
  let confss = S.fromList $ fmap sConf kns
      -- past = S.concatMapWith S.wAsync
      --   (hydrateKbtz' hydrationOpts) confss
      present = S.concatMapWith S.wAsync (S.concatM . runKibbutz @t) confss
  return $ S.mapM (pure . const True) $ present
  --return $ (fmap snd past)
  --  `S.async` (fmap (const True) present)
  where
    kns = [(labKbtz, labNodes), (labKbtz1, labNodes1)]
    futPrefix = "runKibbutz :"
    labKbtz = (KbtzId "Lab Original")
    labKbtz1 = (KbtzId "Lab Latest")
    -- labNodes = NodeId <$> [ "7c:9e:bd:f5:ec:74", "c4:4f:33:67:ea:69"
    --                           , "ac:67:b2:11:e5:c4", "7c:9e:bd:f6:43:88" ]
    -- labNodes = NodeId <$> [ "8c:aa:b5:97:69:48"
    --                       , "ac:67:b2:11:f2:30"
    --                       , "8c:aa:b5:95:97:c8"
    --                       , "ac:67:b2:1c:ec:d8"
    --                       , "c4:4f:33:67:ea:69"
    --                       , "7c:9e:bd:f5:ec:74"
    --                       , "ac:67:b2:11:f0:28"
    --                       , "7c:9e:bd:f6:48:88"
    --                       ]
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
