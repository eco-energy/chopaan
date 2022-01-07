{-# LANGUAGE TypeApplications, ScopedTypeVariables, RecordWildCards, FlexibleContexts, OverloadedStrings, ConstraintKinds #-}
module Chopaan where

import Control.Applicative
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Chopaan.Kibbutz
import Chopaan.Hydration.Prefix
import Chopaan.Hydrate ( hConfDef, mkTKbtz, runHydration )
import Chopaan.Kibbutz.KbtzId ( KbtzId(KbtzId) )
import Chopaan.Kibbutz.FS
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
import Data.Bifunctor ( Bifunctor(..) )
import Data.Pool (stats)
import qualified Data.Map.Strict as M
import Data.Maybe

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

import ConCat.Graphics.Image
import ConCat.Graphics.Color
import ConCat.Synchronous
import Chopaan.Ui

type KbtzM = ReaderT (MQTTOpts) GraphM



runKbtzim :: forall t.
  (S.IsStream t)
  => MQTTOpts
  -> HydrationOpts
  -> InfluxConn
  -> GraphM (t GraphM (Either (NodeMAC, Prefix) (KbtzScene NodeMAC)))
runKbtzim mq hydrationOpts influxCon = do
  kns <- fromJust <$> S.head (kbtzimEnv undefined)
  let 
      s3Hydration :: t GraphM (NodeMAC, Prefix)
      s3Hydration = runHydration influxCon hConfDef kns
      mqttStream = S.concatMapWith S.parallel (S.concatM . runKibbutz @t) (confss kns)
  return $ (Left <$> s3Hydration) `S.parallel` (Right <$> mqttStream)
  where
    confss :: (S.IsStream t, S.MonadAsync m) => Kbtzim -> t m (KbtzC NodeMAC)
    confss = S.fromList . fmap sConf . fmap (second toKbtzG) . M.toList
    sConf (k, kns) = KbtzC { Chopaan.Kibbutz.name = k
                           , structure = kns
                           , channelOpts = Left mq
                           , s3Opts = Just (BucketName (s3BucketName hydrationOpts))
                           , influxCon = influxCon
                           }
    deployKbtz = (KbtzId "Bismillah_Mor", fmap fst deployNodes)
    


type MonConstraint t m n a = ( S.IsStream t, S.MonadAsync m, HasPath n
                           , Renderable n, Renderable a, MonadRender m )

class Renderable a where
  render :: a -> (ImageC, Region)

instance (Renderable a, Renderable b) => Renderable (a, b) where
  render (a, b) = ((liftA2 overC colA colB), unionR regA regB)
    where
      (colA, regA) = render a
      (colB, regB) = render b
  
  
class (Monad m) => MonadRender m where
  renderM :: ImageC -> Region -> m ()
  
newtype RenderM m a = RenderM (m a)


monitor :: forall t m n a. (MonConstraint t m n a) => t m (n, a) -> m ()
monitor = S.foldlM' (\_ a -> (uncurry renderM) . render $ a) (pure ()) . S.adapt 

type NodeKey = NodeId Int

newtype DispatchNodes m n x = DispatchNodes (UF.Unfold m n x) 

run :: RIO App ()
run = do
  hSetBuffering stdout LineBuffering 
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  tc <- liftIO $ execParser tkOptions
  ic <- liftIO $ execParser icOptions
  serverThread <- liftIO $ forkServer "localhost" 8111
  liftIO $ createDB influxConn "chopaanMQTT"
  liftIO $ runGraphM poolConf tc $
    S.drain . S.fromAhead =<< (runKbtzim @S.AheadT mqttOpts hydrationOpts ic)



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

-- labNodes = NodeId <$> [ "ac:67:b2:11:f3:20",
--                         "ac:67:b2:1d:e7:f4",
--                         "8c:aa:b5:97:69:48",
--                         "8c:aa:b5:95:97:c8",
--                         "8c:aa:b5:95:8f:9c",
--                         "ac:67:b2:1c:ec:d8",
--                         "7c:9e:bd:f5:ec:74",
--                         "ac:67:b2:11:f0:28"
--                       ]
-- labNodes1 :: [NodeMAC]
-- labNodes1 = NodeId <$>
--   [ "ac:67:b2:11:f3:10"
--   , "ac:67:b2:12:07:b0"
--   , "7c:9e:bd:47:61:bc"
--   , "7c:9e:bd:47:b7:e8"
--   , "7c:9e:bd:48:4e:e0"
--   , "7c:9e:bd:48:a2:c4"
--   , "ac:67:b2:11:e6:e4"
--   ]
