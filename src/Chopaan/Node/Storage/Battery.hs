{-# LANGUAGE CPP, OverloadedStrings, TypeApplications, BangPatterns, DeriveAnyClass, GeneralisedNewtypeDeriving, NamedFieldPuns, DeriveGeneric, ScopedTypeVariables, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Chopaan.Node.Storage.Battery where

import GHC.Generics
import Control.DeepSeq

import Data.Typeable
import Data.Selectors
import Data.Bifunctor
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as A


import qualified Data.ByteString.Lazy as BL

import Chopaan.Graph.Greskell

import Data.Greskell (Key, lookupAs, pMapToFail
                     , FromGraphSON(..), parseGraphSON)
import Data.Greskell.Extra (writeKeyValues, (<=:>))
import Data.Greskell.GraphSON.GValue (unwrapAll)
import Shpadoinkle.Widgets.Types (Humanize(..))
#ifndef ghcjs_HOST_OS
import NetSpider.Graph (NodeAttributes(..), VFoundNode, LinkAttributes(..), EFinds)
#endif


data Battery e p = Battery
  { soc :: !e
  , chargeLim :: !p
  , dischargeLim :: !p
  , totalCapacity :: !e
  } deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable, Traversable, Humanize)

instance (Typeable e, Typeable p) => Selectors (Battery e p) where
  selectors = selectorsRep @(Battery e p)

instance Bifunctor Battery where
  bimap f g Battery{soc, chargeLim, dischargeLim, totalCapacity} = Battery
    { soc = f soc
    , chargeLim = g chargeLim
    , dischargeLim = g dischargeLim
    , totalCapacity = f totalCapacity
    } 


instance (ToJSON e, ToJSON p) => ToJSON (Battery e p)
instance (FromJSON e, FromJSON p) => FromJSON (Battery e p)

#ifndef ghcjs_HOST_OS
instance (GreskellC e, GreskellC p) => FromGraphSON (Battery e p) where
  parseGraphSON = parseJSON . unwrapAll

batKey :: Key VFoundNode BL.ByteString
batKey = "battKey"

instance (GreskellC e, GreskellC p) => NodeAttributes (Battery e p) where
  writeNodeAttributes bat = fmap writeKeyValues $ sequence $
    [ batKey <=:> A.encode bat ]
  parseNodeAttributes props = pMapToFail $ decodeBin "battery: battery" $ lookupAs batKey props
#endif

emptyB :: (Fractional e, Fractional p) => Battery e p
emptyB = Battery 0 0 0 0

instance (Fractional e, Fractional p, Ord e, Ord p) => Semigroup (Battery e p) where
  b <> b' = (emptyB @e @p) { soc = min (soc b)  (soc b')
                           , chargeLim = min (chargeLim b) (chargeLim b')
                           , dischargeLim = min (dischargeLim b) (dischargeLim b')
                           , totalCapacity = min (totalCapacity b) (totalCapacity b')
                           }

instance (Fractional e, Fractional p, Ord e, Ord p) => Monoid (Battery e p) where
  mempty = emptyB
      

socPercentage :: Fractional e => Battery e p -> e
socPercentage !Battery{soc, totalCapacity} = (soc * 100 / totalCapacity)
