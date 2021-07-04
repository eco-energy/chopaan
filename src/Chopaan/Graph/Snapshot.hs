{-# LANGUAGE DeriveGeneric, DeriveAnyClass, CPP, StandaloneDeriving #-}
-- |
-- Vendoring of (NetSpider is not GHCJS Compat)
-- Module: NetSpider.Snapshot.Internal
-- Description: Implementation of Snapshot graph types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use this.__
--
-- Implementation of Snapshot graph types. This module is for internal
-- and testing purposes only.
--
-- @since 0.3.0.0
module Chopaan.Graph.Snapshot
       ( SnapshotGraph,
         SnapshotLink(..),
         linkNodeTuple,
#ifndef ghcjs_HOST_OS
         fromNSNode,
         fromNSLink,
         fromNSGraph,
#endif
         SnapshotNode(..)
         
       ) where

import Control.DeepSeq (NFData)
import Control.Applicative (many, (*>))
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (Bifunctor(..))
import Data.Char (isUpper, toLower)
import GHC.Generics (Generic)
import qualified Text.Regex.Applicative as RE
import Data.Time

#ifndef ghcjs_HOST_OS
import Data.Time.Clock.System (systemToUTCTime)
import qualified NetSpider.Snapshot as N
import qualified NetSpider.Snapshot.Internal as N
import qualified NetSpider.Timestamp as T
#endif

-- | The snapshot graph, which is a collection nodes and links.
--
-- @since 0.3.1.0
type SnapshotGraph n na la = ([SnapshotNode n na], [SnapshotLink n la])

-- | A link in the snapshot graph.
--
-- 'SnapshotLink' is summary of one or more link observations by
-- different subject nodes. Basically the latest of these observations
-- is used to make 'SnapshotLink'.
--
-- - type @n@: node ID.
-- - type @la@: link attributes.
data SnapshotLink n la =
  SnapshotLink
  { _sourceNode :: n,
    _destinationNode :: n,
    _isDirected :: Bool,
    _linkTimestamp :: UTCTime,
    _linkAttributes :: la
    
    -- Maybe it's a good idea to include 'observationLogs', which can
    -- contain warnings or other logs about making this SnapshotLink.
  }
  deriving (Show,Eq,Generic, NFData)

-- | Comparison by node-tuple (source node, destination node).
instance (Ord n, Eq la) => Ord (SnapshotLink n la) where
  compare l r = compare (linkNodeTuple l) (linkNodeTuple r)

-- | @since 0.3.0.0
instance Functor (SnapshotLink n) where
  fmap f l = l { _linkAttributes = f $ _linkAttributes l }

-- | @since 0.3.0.0
instance Bifunctor SnapshotLink where
  bimap fn fla l = l { _linkAttributes = fla $ _linkAttributes l,
                       _sourceNode = fn $ _sourceNode l,
                       _destinationNode = fn $ _destinationNode l
                     }

aesonOpt :: Aeson.Options
aesonOpt = Aeson.defaultOptions
           { Aeson.fieldLabelModifier = modifier
           }
  where
    modifier = RE.replace reSnake . RE.replace reAttr . RE.replace reDest . RE.replace reTime
    reDest = fmap (const "dest") $ RE.string "destination"
    reAttr = fmap (const "Attrs") $ RE.string "Attributes"
    reTime = fmap (const "timestamp") (many RE.anySym *> RE.string "Timestamp")
    reSnake = RE.msym $ \c ->
      if c == '_'
      then Just ""
      else if isUpper c
           then Just ['_', toLower c]
           else Nothing

-- | @since 0.4.1.0
instance (FromJSON n, FromJSON la) => FromJSON (SnapshotLink n la) where
  parseJSON = Aeson.genericParseJSON aesonOpt

-- | @since 0.4.1.0
instance (ToJSON n, ToJSON la) => ToJSON (SnapshotLink n la) where
  toJSON = Aeson.genericToJSON aesonOpt
  toEncoding = Aeson.genericToEncoding aesonOpt
  

-- | Node-tuple (source node, destination node) of the link.
linkNodeTuple :: SnapshotLink n la -> (n, n)
linkNodeTuple link = (_sourceNode link, _destinationNode link)


-- | A node in the snapshot graph.
data SnapshotNode n na =
  SnapshotNode
  { _nodeId :: n,
    _isOnBoundary :: Bool,
    _nodeTimestamp :: Maybe UTCTime,
    _nodeAttributes :: Maybe na
  }
  deriving (Show,Eq,Generic, NFData)

-- | Comparison by node ID.
instance (Ord n, Eq na) => Ord (SnapshotNode n na) where
  compare l r = compare (_nodeId l) (_nodeId r)

-- | @since 0.3.0.0
instance Functor (SnapshotNode n) where
  fmap f n = n { _nodeAttributes = fmap f $ _nodeAttributes n }

-- | @since 0.3.0.0
instance Bifunctor SnapshotNode where
  bimap fn fna n = n { _nodeAttributes = fmap fna $ _nodeAttributes n,
                       _nodeId = fn $ _nodeId n
                     }

-- | @since 0.4.1.0
instance (FromJSON n, FromJSON na) => FromJSON (SnapshotNode n na) where
  parseJSON = Aeson.genericParseJSON aesonOpt

-- | @since 0.4.1.0
instance (ToJSON n, ToJSON na) => ToJSON (SnapshotNode n na) where
  toJSON = Aeson.genericToJSON aesonOpt
  toEncoding = Aeson.genericToEncoding aesonOpt


#ifndef ghcjs_HOST_OS
deriving instance Generic T.Timestamp
deriving instance NFData T.Timestamp
#endif

#ifndef ghcjs_HOST_OS
toUTC = systemToUTCTime . T.toSystemTime

toNSLink :: SnapshotLink n la -> N.SnapshotLink n la
toNSLink s
  = N.SnapshotLink
    (_sourceNode s)
    (_destinationNode s)
    (_isDirected s)
    (T.fromUTCTime $ _linkTimestamp s)
    (_linkAttributes s)

fromNSLink :: N.SnapshotLink n la -> SnapshotLink n la
fromNSLink s
  = SnapshotLink
    (N._sourceNode s)
    (N._destinationNode s)
    (N._isDirected s)
    (toUTC $ N._linkTimestamp s)
    (N._linkAttributes s)

toNSNode :: SnapshotNode n la -> N.SnapshotNode n la
toNSNode v = N.SnapshotNode (_nodeId v)
    (_isOnBoundary v)
    (fmap T.fromUTCTime $ _nodeTimestamp v)
    (_nodeAttributes v)

fromNSNode :: N.SnapshotNode n la -> SnapshotNode n la
fromNSNode v = SnapshotNode (N._nodeId v)
    (N._isOnBoundary v)
    (fmap toUTC $ N._nodeTimestamp v)
    (N._nodeAttributes v)

fromNSGraph :: N.SnapshotGraph n na la -> SnapshotGraph n na la
fromNSGraph = bimap (fmap fromNSNode) (fmap fromNSLink)
#endif
