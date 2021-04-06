{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveFunctor #-}
module Chopaan.Node.NodeId where

import Servant.API
import Data.Hashable (Hashable(..))
import Data.Csv (ToField(..))
import GHC.Generics
import qualified Data.Text as Text
import Diagrams.Names
import Data.Typeable
import Data.Aeson
import Control.DeepSeq (NFData)
import Shpadoinkle.Widgets.Types (Humanize (..))

type ThingName = Text.Text

type NodeMAC = NodeId ThingName

newtype NodeId a = NodeId { unNodeId :: a }
  deriving (Eq, Ord, Generic, Typeable, FromJSON, ToJSON, NFData, Functor)

instance (Show a) => Show (NodeId a) where
  show (NodeId a) = show a

instance (Hashable a) => Hashable (NodeId a)

instance (ToField a) => ToField (NodeId a) where
  toField (NodeId a) = toField a 

instance (FromHttpApiData a) => FromHttpApiData (NodeId a) where
  parseUrlPiece text = NodeId <$> (parseUrlPiece text)

instance (ToHttpApiData a) => ToHttpApiData (NodeId a) where
  toUrlPiece (NodeId ns) = (toUrlPiece ns)

instance (Typeable a, Ord a, Show a) => IsName (NodeId a)

deriving instance (Show a, Humanize a) => Humanize (NodeId a)
