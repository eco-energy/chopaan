{-# LANGUAGE KindSignatures, TypeOperators, DataKinds, FlexibleContexts, TypeFamilies, FlexibleInstances, LambdaCase, TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances, InstanceSigs, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving
, DerivingStrategies, DeriveAnyClass, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.Node.NodeT where

import GHC.Generics
import Control.Monad.Identity (Identity)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)

import Database.Beam (Beamable, Columnar) --Table (..), TableEntity)


import Shpadoinkle.Widgets.Types (Field, Humanize (..)
                                 , Hygiene (Clean)
                                 , Input (Input)
                                 , Status (Edit, Errors, Valid)
                                 , Validate (rules))

import Chopaan.Node.NodeId
import Chopaan.Node.HW


data NodeT f = Node
  { _nodeId :: Columnar f (NodeId Int)
  , _nodeMAC :: Columnar f (NodeMAC)
  , _hardwareConfig :: Columnar f (HW)
  } deriving (Generic, Beamable)

deriving instance Eq (NodeT Identity)
deriving instance Ord (NodeT Identity)
deriving instance Show (NodeT Identity)
instance NFData (NodeT Identity)
deriving instance ToJSON (NodeT Identity)
deriving instance FromJSON (NodeT Identity)

deriving instance Humanize (NodeT Identity)

type Nodezim = NodeT Identity


data NodeUpdate s = NodeUpdate
  { _hardwareConfigU :: Field s Text Input HW
  } deriving (Generic)

emptyNodeForm :: NodeUpdate 'Edit
emptyNodeForm = NodeUpdate
  { _hardwareConfigU = Input Clean defHW
  }

instance (NFData (Field s Text Input HW)) => NFData (NodeUpdate s)

deriving instance Eq       (NodeUpdate 'Valid)
deriving instance Ord      (NodeUpdate 'Valid)
deriving instance Show     (NodeUpdate 'Valid)
deriving instance ToJSON   (NodeUpdate 'Valid)
deriving instance FromJSON (NodeUpdate 'Valid)

deriving instance Eq       (NodeUpdate 'Edit)
deriving instance Ord      (NodeUpdate 'Edit)
deriving instance Show     (NodeUpdate 'Edit)
deriving instance ToJSON   (NodeUpdate 'Edit)
deriving instance FromJSON (NodeUpdate 'Edit)

deriving instance Show     (NodeUpdate 'Errors)


instance Validate NodeUpdate where
  rules = NodeUpdate
    { _hardwareConfigU = pure
    }
