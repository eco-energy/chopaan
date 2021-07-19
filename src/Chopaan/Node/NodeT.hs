{-# LANGUAGE KindSignatures, TypeOperators, DataKinds, FlexibleContexts, TypeFamilies, FlexibleInstances, LambdaCase, TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances, InstanceSigs, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving
, DerivingStrategies, DeriveAnyClass, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings, CPP #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Chopaan.Node.NodeT where

import GHC.Generics
import Control.Monad.Identity (Identity)
import Control.Monad.Except
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding


import Database.Beam (Beamable, Columnar)


import Shpadoinkle.Widgets.Types (Field, Humanize (..)
                                 , Hygiene (Clean)
                                 , Input (Input), getValue
                                 , Status (Edit, Errors, Valid)
                                 , Validate (..), Validated(..), ValidateG(..))
import Shpadoinkle.Widgets.Validation ( between
                                      , nonMEmpty
                                      , nonZero
                                      , positive)
#include "UpdateInst.inc"
import Chopaan.Node.NodeId
import Chopaan.Node.HW


data NodeT f = Node
  { _nodeId :: Columnar f (NodeId Int)
  , _nodeMAC :: Columnar f (NodeMAC)
  , _hardwareConfig :: Columnar f (HW Double)
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
  { nodeMACU :: Field s Text Input (NodeMAC)
  , hardwareConfigU :: HWUpdate s
  } deriving (Generic)

instance ( NFDataHW s
         , NFData (Field s Text Input NodeMAC)
         ) => NFData (NodeUpdate s)

UpdateInstances(NodeUpdate)


emptyNodeForm :: NodeUpdate 'Edit
emptyNodeForm = NodeUpdate
  { nodeMACU = Input Clean (NodeId "")
  , hardwareConfigU = emptyHWForm
  }


isValidMAC :: NodeMAC -> Validated Text NodeMAC 
isValidMAC (NodeId mac) = if parsesTrue mac
                 then pure $ NodeId mac
                 else throwError "Not a valid MAC Address"
  where
    parsesTrue :: Text -> Bool
    parsesTrue t = all (\x -> (T.length x) == 2) (T.splitOn ":" t)


instance Validate NodeUpdate where
  rules = NodeUpdate
    { nodeMACU = isValidMAC
    , hardwareConfigU = rules
    }
  validate (NodeUpdate{nodeMACU, hardwareConfigU}) = NodeUpdate
    { nodeMACU = (isValidMAC . getValue $ nodeMACU) 
    , hardwareConfigU = validate hardwareConfigU
    }
  getValid (NodeUpdate{nodeMACU, hardwareConfigU}) = case getValid hardwareConfigU of
    Nothing -> Nothing
    Just x -> case nodeMACU of
      Validated a -> Just (NodeUpdate {nodeMACU = a, hardwareConfigU = x})
      _ -> Nothing
