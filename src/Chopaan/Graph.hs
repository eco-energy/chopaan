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

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq
import Data.Aeson (ToJSON, FromJSON)
import Data.Greskell (FromGraphSON)

#ifndef ghcjs_HOST_OS
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader hiding (ask)
import Control.Monad.Reader.Class
import Control.Monad.Catch
import Control.Monad.Base
import Control.Monad.Trans.Control

import Chopaan.Graph.Spider
import Chopaan.Graph.Kbtz
import Network.Greskell.WebSocket (Client)
import Data.Pool
#endif
import Shpadoinkle.Widgets.Types (Humanize)

import Chopaan.Graph.G
import Chopaan.Graph.Snapshot



#ifndef ghcjs_HOST_OS
newtype GraphM a = GraphM { runGraphM :: ReaderT (DBPools) IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (DBPools),
                    MonadBase IO, MonadBaseControl IO, MonadThrow, MonadCatch)

toHandlerH :: MonadIO m => String -> Int -> GraphM ~> m
toHandlerH h p a = liftIO $ runReaderT (runGraphM a) =<< (mkDBPools h p)

mkDBPools :: MonadIO m => String -> Int -> m (DBPools)
mkDBPools h p = do
  kp <- liftIO $ kbtzPool h p
  spools <- liftIO $ mkSpool $ mkConfG (h, p)
  return $ DBPools spools kp

data DBPools = DBPools
  { spools :: Spools
  , gremlinPool :: KbtzPool
  }

withKbtzPool :: (Client -> GraphM a) -> GraphM a
withKbtzPool f = do
    (DBPools _ kp) <- ask
    withResource kp f

withSpider :: SpiderM ~> GraphM
withSpider f = (\s -> runSpider s f) =<< (fmap spools ask)
#endif



data GraphType = MeshG | PlanG | StatusG | FlowG
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, ToJSON, FromJSON, NFData, Humanize)
