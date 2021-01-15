{-# LANGUAGE DeriveGeneric #-}
module Chopaan.Kibbutz.KbtzId where

import Servant
import Data.Hashable (Hashable(..))
import Data.Csv (ToField(..))
import GHC.Generics
import qualified Data.Text as Text
import Diagrams.Names
import Data.Typeable


newtype KbtzId a = KbtzId { unKbtzId :: a } deriving (Eq, Ord, Show, Generic, Typeable)

instance (Hashable a) => Hashable (KbtzId a)

instance (ToField a) => ToField (KbtzId a) where
  toField (KbtzId a) = toField a

instance (FromHttpApiData a) => FromHttpApiData (KbtzId a) where
  parseUrlPiece text = KbtzId <$> (parseUrlPiece text)

instance (ToHttpApiData a) => ToHttpApiData (KbtzId a) where
  toUrlPiece (KbtzId ns) = (toUrlPiece ns)

instance (Typeable a, Ord a, Show a) => IsName (KbtzId a)

type KbtzName = KbtzId Text.Text
