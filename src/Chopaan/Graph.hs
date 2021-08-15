{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances, DeriveAnyClass, DerivingStrategies, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ScopedTypeVariables, TypeApplications, FlexibleContexts, TypeOperators, GADTs #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies, InstanceSigs
, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints
, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase, TypeOperators, TypeApplications, CPP #-}
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
import Control.DeepSeq
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (pack, Text)

#ifndef ghcjs_HOST_OS
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader hiding (ask)
import Control.Monad.Reader.Class
import Control.Monad.Catch
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.IO.Unlift
import Chopaan.Graph.Spider
import Chopaan.Graph.Kbtz
import Network.Greskell.WebSocket (Client)
import Data.Pool
import Network.AWS.S3 (BucketName(..))
#endif
import Shpadoinkle.Widgets.Types (Humanize)

import Chopaan.Graph.G


#ifndef ghcjs_HOST_OS
newtype GraphM a = GraphM { runGraphM' :: ReaderT (DBPools) IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (DBPools),
                    MonadBase IO, MonadBaseControl IO, MonadThrow, MonadCatch, MonadUnliftIO)

runGraphM :: MonadIO m => String -> Int -> GraphM ~> m
runGraphM h p a = liftIO $ runReaderT (runGraphM' a) =<< (mkDBPools h p)

runGraphWithDB :: MonadIO m => DBPools -> GraphM ~> m
runGraphWithDB db = liftIO . (flip runReaderT db) . runGraphM'

mkDBPools :: MonadIO m => String -> Int -> m (DBPools)
mkDBPools h p = do
  kp <- liftIO $ kbtzPool h p
  spools <- liftIO $ mkSpool $ mkConfG (h, p)
  return $ DBPools spools kp (BucketName b)
    where
      b = "dosti-datastream"

data DBPools = DBPools
  { spools :: Spools
  , gremlinPool :: KbtzPool
  , s3Bucket :: BucketName
  }

withKbtzPool :: (Client -> GraphM a) -> GraphM a
withKbtzPool f = do
    (DBPools _ kp _) <- ask
    withResource kp f

withSpider :: SpiderM ~> GraphM
withSpider f = (\s -> runSpider s f) =<< (fmap spools ask)
#endif
