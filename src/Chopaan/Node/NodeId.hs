{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}


module Chopaan.Node.NodeId where

import qualified Codec.Winery as W
import Control.DeepSeq (NFData)
import Data.Aeson ( FromJSON, ToJSON, ToJSONKey )
import Data.Binary ( Binary )
import Data.Csv (ToField (..))
import Data.Greskell (FromGraphSON)
import Data.Hashable (Hashable (..))
import Data.String ( IsString )
import qualified Data.Text as Text
import Data.Typeable ( Typeable )
import Diagrams.Names ( IsName )
import GHC.Generics ( Generic )
import Servant.API
    ( FromHttpApiData(parseUrlPiece), ToHttpApiData(toUrlPiece) )

type ThingName = Text.Text

type NodeMAC = NodeId ThingName

type NodeIdx = NodeId Int

newtype NodeId a = NodeId {unNodeId :: a}
  deriving stock (Generic, Functor)
  deriving newtype (Eq, Ord, Show, Read, IsString, Typeable, FromJSON, ToJSON, Semigroup, Monoid, FromGraphSON, ToJSONKey)
  deriving anyclass (NFData, Binary)
  deriving (W.Serialise) via (W.WineryRecord (NodeId a))

instance (Hashable a) => Hashable (NodeId a)

instance (ToField a) => ToField (NodeId a) where
  toField (NodeId a) = toField a

instance (FromHttpApiData a) => FromHttpApiData (NodeId a) where
  parseUrlPiece text = NodeId <$> parseUrlPiece text

instance (ToHttpApiData a) => ToHttpApiData (NodeId a) where
  toUrlPiece (NodeId ns) = toUrlPiece ns

instance (Typeable a, Ord a, Show a) => IsName (NodeId a)

toText :: (Show a) => NodeId a -> Text.Text
toText = Text.replace "\"" "" . Text.pack . show . unNodeId
