{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances, DeriveAnyClass, DerivingStrategies, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ScopedTypeVariables, TypeApplications, FlexibleContexts, TypeOperators, GADTs #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies, InstanceSigs
, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints
, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase, TypeOperators, TypeApplications, CPP, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Graph ( module Chopaan.Graph
                     , module Chopaan.Graph.G
#ifndef ghcjs_HOST_OS
                     , module Chopaan.Graph.Spider
#endif
                     ) where

import Prelude
import Control.Monad

import GHC.Generics (Generic, Generic1)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.DeepSeq
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (pack, Text)

import Chopaan.Kibbutz.KbtzId (KbtzName)
import Chopaan.Node.NodeId (NodeMAC)

import Data.Functor.Compose
#ifndef ghcjs_HOST_OS
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader hiding (ask)
import Control.Monad.Reader.Class
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.IO.Unlift
import Chopaan.Graph.Spider
import Chopaan.Graph.Kbtz
import Network.Greskell.WebSocket (Client)
import Data.Pool
import Network.AWS.S3 (BucketName(..))
import Chopaan.Types (PoolConf(..))
import qualified System.Envy as E
import System.Random.MWC
import qualified System.Random.MWC.Distributions as MWC

import Options.Applicative
--import Servant.Server (ServerError)
#endif
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
-- import Shpadoinkle.Widgets.Types (Humanize)

import Chopaan.Graph.G


newtype Context = Context { unContext :: (DBPools, GenIO) }
  deriving Generic

getPools :: Context -> DBPools
getPools = fst . unContext

getGen :: Context -> GenIO
getGen = snd . unContext

genCtxt :: MonadIO m => DBPools -> m Context
genCtxt p = Context <$> ((p, ) <$> sysRand)

#ifndef ghcjs_HOST_OS
newtype GraphM a = GraphM { runGraphM' :: ReaderT Context IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Context),
                    MonadBase IO, MonadBaseControl IO, MonadThrow, MonadCatch,
                    MonadMask, MonadUnliftIO)
  
--fromSamplerST :: SamplerST a 

instance MonadSample GraphM where
  random = (liftIO . sampleIOwith random)
    =<< (getGen <$>  ask)
  {-# INLINE random #-}

runGraphM :: MonadIO m => PoolConf -> TinkerConf -> GraphM ~> m
runGraphM pc (TinkerConf h p) a = do
  c <- (,) <$> (mkDBPools pc h p) <*> sysRand
  liftIO $ runReaderT (runGraphM' a) (Context c)
{-# INLINE runGraphM #-}

runGraphWithDB :: MonadIO m => DBPools -> GraphM ~> m
runGraphWithDB db a = liftIO $ (runReaderT  (runGraphM' a)) =<< genCtxt db
{-# INLINE runGraphWithDB #-}

mkDBPools :: MonadIO m => PoolConf -> String -> Int -> m (DBPools)
mkDBPools pc h p = do
  kp <- liftIO $ kbtzPool h p
  spools <- liftIO $ mkSpool pc $ mkConfG (h, p)
  return $ DBPools spools kp (BucketName b)
    where
      b = "dosti-datastream"
{-# INLINE mkDBPools #-}

data DBPools = DBPools
  { spools :: Spools
  , gremlinPool :: KbtzPool
  , s3Bucket :: BucketName
  }

withKbtzPool :: (Client -> GraphM a) -> GraphM a
withKbtzPool f = do
    (DBPools _ kp _) <- getPools <$> ask
    withResource kp f
{-# INLINE withKbtzPool #-}

withSpider :: SpiderM ~> GraphM
withSpider f = (\s -> runSpider s f) =<< ((spools . getPools) <$> ask)
{-# INLINE withSpider #-}

hoistG :: forall t m. (S.IsStream t, S.MonadAsync m) => DBPools -> (t GraphM) ~> (t m) 
hoistG db = S.adapt . S.hoist (runGraphWithDB db) . S.adapt
{-# INLINE hoistG #-}

data TinkerConf = TinkerConf
  { janusHost :: String
  , janusPort :: Int
  } deriving (Generic, E.FromEnv)


tkParser :: Parser TinkerConf
tkParser = TinkerConf
  <$> strOption   (long "tinkerHost" <> metavar "TINKERHOST")
  <*> option auto (long "tinkerPort" <> metavar "TINKERPORT" <> showDefault <> value 8182)

tkOptions :: ParserInfo TinkerConf
tkOptions = info (tkParser <**> helper) $
    fullDesc <> progDesc "Chopaan"
             <> header "Control and Monitor Kbtzim"

getKNs :: GraphM ([(KbtzName, [NodeMAC])])
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


sysRand :: MonadIO m => m GenIO
sysRand = liftIO createSystemRandom
#endif

