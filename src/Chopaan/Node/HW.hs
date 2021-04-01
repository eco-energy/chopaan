{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
module Chopaan.Node.HW where

import GHC.Generics

import Proto.NodeMessageSchema.NodeMessages (HardwareConfig)
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N

import Lens.Micro

import Data.Aeson (ToJSON, FromJSON)
import Control.DeepSeq (NFData)
import Data.Greskell (Key, lookupAs, pMapToFail)
import Data.Greskell.Extra (writeKeyValues, (<=:>))

import NetSpider.Found (FoundNode(..), FoundLink(..))
import NetSpider.Graph (LinkAttributes(..), NodeAttributes(..), VFoundNode, EFinds)
import Shpadoinkle.Widgets.Types (Humanize(..))

import Chopaan.Node.Components

data Wire = Wire
  { distance :: Double
  , gauge :: Double
  } deriving (Eq, Ord, Show, Generic)



data HW = HW
  { storage :: BatteryTop Double
  , generation :: PVTop Double
  , loads :: LoadTop Double
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

defHW :: HW
defHW = HW (SingBC BatteryConf) (SingPC PVConf) (SingLC LoadConf)

instance Semigroup HW
instance Monoid HW

instance Humanize HW

instance NodeAttributes HW where
  writeNodeAttributes hw = fmap writeKeyValues $ sequence $
    [ 
    ]
  parseNodeAttributes = undefined --pMapToFail


distanceKey :: Key EFinds Double
distanceKey = "distance"

gaugeKey :: Key EFinds Double
gaugeKey = "gauge"

instance LinkAttributes Wire where
  writeLinkAttributes w = fmap writeKeyValues $ sequence $
    [ distanceKey <=:> distance w
    , gaugeKey <=:> gauge w
    ] 
  parseLinkAttributes props = pMapToFail (Wire
                                          <$> lookupAs distanceKey props
                                          <*> lookupAs gaugeKey props
                                         )

