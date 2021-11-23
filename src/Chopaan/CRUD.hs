{-# LANGUAGE KindSignatures, TypeOperators, DataKinds, FlexibleContexts, TypeFamilies, FlexibleInstances, LambdaCase, TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances, InstanceSigs, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving
, DerivingStrategies, DeriveAnyClass, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints, InstanceSigs, CPP #-}
module Chopaan.CRUD where

import GHC.Generics

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import Data.Function (on)
import Data.Time (UTCTime)
import Data.Text (Text)

import qualified Data.Map as M (Map)

-- import Shpadoinkle (Html, MonadJSM)
-- import Shpadoinkle.Widgets.Table (Tabular(..), Column, Row, SortCol(..), Sort(..))
-- import Shpadoinkle.Widgets.Types (Humanize (..), Present(present))

import Streamly.Prelude (IsStream, adapt)
import Streamly.Internal.Data.Stream.IsStream (hoist)

import Control.Monad.Trans.Class
import Chopaan.Node.NodeId
import Chopaan.Node.Mesh (MeshNode, RxSignal)
import Chopaan.Node.Folds (SensorR)


import Chopaan.Hydration.Prefix (Resolution(..))
import Chopaan.Kibbutz.KbtzId
import Chopaan.Graph
import Data.Time
