{-# LANGUAGE TypeApplications, ScopedTypeVariables, RecordWildCards, FlexibleContexts, OverloadedStrings, ConstraintKinds #-}
module Chopaan where

import Control.Applicative
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Chopaan.Kibbutz
import Chopaan.Hydration.Prefix
import Chopaan.Hydrate ( hConfDef, runHydration, HConS)
import Chopaan.Kibbutz.TKbtzim (mkConfig, TKbtzim, onEvT)
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
import qualified Streamly.Internal.Data.Stream.IsStream.Generate as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.FileSystem.Handle as H
import qualified Streamly.Internal.FileSystem.File as File


import System.Remote.Monitoring (forkServer)

import Network.AWS.S3 (BucketName(..))
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
import Path.IO
import Path


type Sources = (MQTTOpts, HydrationOpts)
type Sink = InfluxConn 

runKbtzim :: forall t m.
  (HConS t m, KConS t m)
  => Ti.UTCTime
  -> MQTTOpts
  -> HydrationOpts
  -> InfluxConn
  -> TKbtzim
  -> t m (NodeMAC, Prefix)
runKbtzim t0 mq hydrationOpts influxCon tKbtzim = s3Hydration tKbtzim
    -- `S.parallel`
    -- (Right <$> mqttStream kbtzim)
  where
    s3Hydration :: TKbtzim -> t m (NodeMAC, Prefix)
    s3Hydration kns = S.concat $ S.unfold (runHydration t0 influxCon hConfDef kns) ()
    -- mqttStream :: Kbtzim -> t m (KbtzScene NodeMAC)
    -- mqttStream kns = S.concatMapWith S.parallel (runKibbutz @t) (confss kns)
    -- confss :: (S.IsStream t, S.MonadAsync m) => Kbtzim -> t m (KbtzC NodeMAC)
    -- confss = S.fromList . fmap (sConf . second toKbtzG) . M.toList
    -- sConf (k, kns) = KbtzC { Chopaan.Kibbutz.name = k
    --                        , structure = kns
    --                        , channelOpts = Left mq
    --                        , s3Opts = Just (BucketName (s3BucketName hydrationOpts))
    --                        , influxCon = influxCon
    --                        }



run :: RIO App ()
run = do
  hSetBuffering stdout LineBuffering
  app <- ask
  let
    Options{..} = appOptions app
  tc <- liftIO $ execParser tkOptions
  --ic <- liftIO $ execParser icOptions
  dir <- makeAbsolute =<< parseRelDir "data/kbtzim" -- liftIO $ getXdgDir XdgData . Just =<< 
  liftIO . print $ "Chopaan Kbtzim Path: " <> (show dir) 
  liftIO $ ensureDir dir
  --liftIO $ createDB influxConn mqttDB
  liftIO $ createDB influxConn hydrationDB
  t0 <- liftIO Ti.getCurrentTime
  liftIO $ runGraphM poolConf tc $ do
    kbtzim0 <- readKbtzim dir
    let kEvs = watchKbtzim @GraphM dir
    tKbtzim <- atomically $ mkConfig kbtzim0
    let wk = S.mapM (onEvT tKbtzim) $ S.trace (liftIO . print) kEvs --  
        rk = (S.fromAhead $ runKbtzim @S.AheadT t0 mqttOpts hydrationOpts influxConn tKbtzim)
    S.drain $ (fmap (const ()) $ S.trace (liftIO . print) rk) `S.parallel` wk
  where
    --mqttDB = "chopaanMQTT"
    hydrationDB = "chopaanS3"
