{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving
, DerivingStrategies, StandaloneDeriving, DeriveFunctor, DerivingVia #-}
module Chopaan.Kibbutz.KbtzId where

import Servant.API
import Data.Hashable (Hashable(..))
import Data.Csv (ToField(..))
import GHC.Generics
import qualified Data.Text as Text
import Diagrams.Names
import Data.Typeable
import Data.Aeson
import Control.DeepSeq (NFData)
-- import Shpadoinkle.Widgets.Types (Humanize (..))
import Data.Greskell (FromGraphSON)
import qualified Codec.Winery as W

type KbtzName = KbtzId Text.Text

newtype KbtzId a = KbtzId { unKbtzId :: a }
  deriving (Eq, Ord, Generic, Typeable, Functor)
  deriving newtype (NFData, FromJSON, ToJSON, FromGraphSON)
  deriving (W.Serialise) via (W.WineryRecord (KbtzId a))

instance (Show a) => Show (KbtzId a) where
  show (KbtzId a) = show a

instance (Hashable a) => Hashable (KbtzId a)

instance (ToField a) => ToField (KbtzId a) where
  toField (KbtzId a) = toField a

instance (FromHttpApiData a) => FromHttpApiData (KbtzId a) where
  parseUrlPiece text = KbtzId <$> (parseUrlPiece text)

instance (ToHttpApiData a) => ToHttpApiData (KbtzId a) where
  toUrlPiece (KbtzId ns) = (toUrlPiece ns)

instance (Typeable a, Ord a, Show a) => IsName (KbtzId a)

--deriving instance (Show a a) => Humanize (KbtzId a)
