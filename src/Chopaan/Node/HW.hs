{-# LANGUAGE DeriveGeneric #-}
module Chopaan.Node.HW where

import GHC.Generics

import Proto.NodeMessageSchema.NodeMessages (HardwareConfig)
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N

import Lens.Micro

import Data.Greskell (Key, lookupAs, pMapToFail)
import Data.Greskell.Extra (writeKeyValues, (<=:>))

import NetSpider.Found (FoundNode(..), FoundLink(..))
import NetSpider.Graph (LinkAttributes(..), NodeAttributes(..), VFoundNode)

data Wire = Wire
  { distance :: Double
  , gauge :: Double
  } deriving (Eq, Ord, Show, Generic)

newtype HW = HW HardwareConfig deriving (Eq, Ord, Show, Generic)

instance NodeAttributes HW where
  writeNodeAttributes hw = fmap writeKeyValues $ sequence $
    [ 
    ]
  parseNodeAttributes = undefined --pMapToFail


instance LinkAttributes Wire where
  writeLinkAttributes = undefined --fmap writeKeyValues wire 
  parseLinkAttributes = undefined

