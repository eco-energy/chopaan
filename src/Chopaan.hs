{-# LANGUAGE TypeApplications, ScopedTypeVariables, RecordWildCards, FlexibleContexts, OverloadedStrings, ConstraintKinds, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveGeneric, UndecidableInstances, TypeOperators, RankNTypes #-}
module Chopaan where

import GHC.Generics
import Control.Applicative
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Reader.Class
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.IO.Unlift
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler

import Chopaan.Kibbutz
import Chopaan.Hydration.Prefix
import Chopaan.Hydrate ( hConfDef, runHydration, HConS, HConM)
import Chopaan.Kibbutz.TKbtzim (mkConfig, TKbtzim(..), onEvT, getKbtzimHW)
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
    ( GraphM, runGraphM, withKbtzPool, tkOptions, getKNs, addzim, type (~>) )
import Data.Influxable (createDB, wp, WriteParams)
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
import System.Random.MWC
import Network.AWS.S3 (BucketName(..))
import qualified Streamly.Internal.Data.Stream.IsStream as S

import Options.Applicative ( execParser )
import RIO
    ( MonadIO(liftIO),
      stdout,
      ReaderT,
      runReaderT,
      MonadReader(ask),
      BufferMode(LineBuffering),
      RIO,
      readTVar,
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
  (HConM m, MonadSample m)
  => Ti.UTCTime
  -> MQTTOpts
  -> WriteParams
  -> TKbtzim
  -> m ()
runKbtzim t0 mq influxCon tKbtzim = s3Hydration tKbtzim
    -- `S.parallel`
    -- (Right <$> mqttStream kbtzim)
  where
    s3Hydration :: TKbtzim -> m ()
    s3Hydration kns = runHydration t0 influxCon kns
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

data Ctx = Ctx
  { root :: AbsDir
  , genIO :: GenIO
  } deriving (Generic)

newtype ChopaanFS a = ChopaanFS { runChopaanFS :: ReaderT Ctx IO a  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Ctx,
                    MonadBase IO, MonadBaseControl IO, MonadFail, MonadThrow, MonadCatch,
                    MonadMask, MonadUnliftIO)

runChopaanM :: MonadIO m => Ctx -> ChopaanFS ~> m
runChopaanM c a = liftIO $ runReaderT (runChopaanFS a) c

instance MonadSample ChopaanFS where
  random = (liftIO . sampleIOwith random) . genIO =<< ask
  {-# INLINE random #-}

run :: RIO App ()
run = do
  hSetBuffering stdout LineBuffering
  app <- ask
  let
    Options{..} = appOptions app
  dir <- makeAbsolute =<< parseRelDir "data/kbtzim" -- liftIO $ getXdgDir XdgData . Just =<< 
  liftIO . print $ "Chopaan Kbtzim Path: " <> (show dir) 
  liftIO $ ensureDir dir
  --liftIO $ createDB influxConn mqttDB
  liftIO $ createDB influxConn hydrationDB
  t0 <- liftIO Ti.getCurrentTime
  gen <- liftIO createSystemRandom
  liftIO $ runChopaanM (Ctx dir gen) $ do
    flip runReaderT dir $ do
      tKbtzim <- atomically . mkConfig =<< readKbtzim
      liftIO $ print =<< (atomically . getKbtzimHW $ tKbtzim)
      let
        dbWrite = wp influxConn hydrationDB
        wk = S.mapM_ (onEvT tKbtzim) $ S.trace (liftIO . print) watchKbtzim 
        rk = runKbtzim t0 mqttOpts dbWrite tKbtzim
      S.drain $ (S.fromEffect rk)
         `S.async`
         (S.fromEffect wk)
  where
    --mqttDB = "chopaanMQTT"
    hydrationDB = "chopaanS3"
