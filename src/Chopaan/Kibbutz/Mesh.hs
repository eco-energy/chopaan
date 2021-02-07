{-# LANGUAGE DeriveGeneric, DeriveFunctor, GeneralizedNewtypeDeriving, DeriveFoldable, DeriveTraversable, DerivingStrategies, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators, TypeApplications, RankNTypes, FlexibleContexts #-}
module Chopaan.Kibbutz.Mesh where

import Prelude hiding (id, (.), curry, uncurry)
import Data.ProtoLens
import Lens.Micro
import qualified Proto.NodeMessageSchema.NodeMessages as N
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N
import GHC.Generics

import ConCat.Category
import ConCat.Misc

import Data.Time (DiffTime)
import Data.Text
import Data.Tree
import Control.Applicative
import Control.Monad

import Chopaan.Comm.Comm (Address(..))

import qualified Streamly.Prelude as S
import Streamly
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

{--
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.isRoot' @:: Lens' RuntimeStats Prelude.Bool@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.connectedChildren' @:: Lens' RuntimeStats Data.Word.Word32@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.wifiStrength' @:: Lens' RuntimeStats Data.Word.Word32@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.meshParentStrength' @:: Lens' RuntimeStats Data.Word.Word32@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.version' @:: Lens' RuntimeStats Data.Text.Text@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.uptime' @:: Lens' RuntimeStats Data.Word.Word64@
 -}

data MeshNode n = MeshNode { isRoot :: Bool, children :: [n], routerRSSI :: Int, parentRSSI :: Int, version :: Text, uptime :: DiffTime}

fromRTS :: N.RuntimeStats -> MeshNode n
fromRTS rts = MeshNode (rts ^. N.isRoot) [] (fromIntegral $ rts ^. N.wifiStrength) (fromIntegral $ rts ^. N.meshParentStrength) (rts ^. N.version) (fromIntegral $ rts ^. N.uptime)
{--(fromIntegral $ rts ^. N.connectedChildren)--}


toTree :: forall t m n. (IsStream t, MonadAsync m, Address n) => t m (n, N.RuntimeStats) -> t m (MeshT (MeshNode n))
toTree = S.postscan (ting)
  where
    ting :: FL.Fold m (n, N.RuntimeStats) (MeshT (MeshNode n))
    ting = FL.Fold next start end
      where
        next :: MeshT (MeshNode n) -> (n, N.RuntimeStats) -> m (MeshT (MeshNode n))
        next (MeshT ptree) (n, rts) = pure . MeshT $ ptree
        start :: m (MeshT (MeshNode n))
        start = pure undefined
        end :: MeshT (MeshNode n) -> m (MeshT (MeshNode n))
        end = pure
        unfolder :: (b -> m ((MeshNode n), [b])) -> b -> m (Tree (MeshNode n))
        unfolder = unfoldTreeM

data Node a = Root a | Child a deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Applicative Node where
  pure = Root
  (Root f) <*> (Root a) = Root $ f a
  (Root f) <*> (Child a) = Root $ f a
  (Child f) <*> (Root a) = Child $ f a
  (Child f) <*> (Child a) = Child $ f a


newtype MeshT a = MeshT { unMeshT :: Tree a }
  deriving (Eq, Show, Generic, Functor, Applicative, Monad, Foldable, Traversable) 

data CommStats = CommStats
  { nothing :: ()
  }

newtype Effect a b = Effect { unEffect :: forall f. (Applicative f) => a -> f b }

data MeshD a b = MeshD
  { structure :: forall t. Traversable t => t a
  , effect :: Effect a b
  }

composeEffect :: (Effect a b) -> (Effect b c) -> Effect a c
composeEffect a b = b . a

composeStructure :: (Traversable t) => t a -> t b -> t b
composeStructure = undefined

affect :: (Applicative f) => MeshD a b -> (a -> f b)
affect = unEffect . effect

coprod :: (Traversable t, Applicative f) => MeshD a b -> f (t b)
coprod f = traverse (affect f) (structure f)

--terminal :: MeshD k a b -> b
--terminal m = traverse . ((affect :+ structure m)) 

instance Category Effect

instance Category (MeshD) where
  id = id
  m1 . m0 = MeshD { structure = structure m0
                  , effect = eff
                  }
            where
              eff = composeEffect (effect m0) (effect m1)

newtype Mesh a = Mesh { unMesh :: forall f b. (Applicative f) => a -> f b }

messageRoute :: (Applicative f) => (a -> f b) -> MeshT a -> f (MeshT b)
messageRoute mesh nodes = traverse mesh nodes  

type RSSI = Int

connectionStrengths :: MeshT (MeshNode n) -> MeshT RSSI
connectionStrengths = undefined

cpuLoadCheck :: Mesh a -> Int -> [a]
cpuLoadCheck = undefined
