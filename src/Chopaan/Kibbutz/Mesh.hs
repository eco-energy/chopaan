{-# LANGUAGE DeriveGeneric, DeriveFunctor, GeneralizedNewtypeDeriving, DeriveFoldable, DeriveTraversable, DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators, TypeApplications #-}
module Chopaan.Kibbutz.Mesh where

import Data.ProtoLens
import qualified Proto.NodeMessageSchema.NodeMessages as N
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N
import GHC.Generics

import ConCat.Misc

import Data.Tree
import Control.Applicative

{--
data Node a = Root a | Child a deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Applicative Node where
  pure = Root
  (Root f) <*> (Root a) = Root $ f a
  (Root f) <*> (Child a) = Child $ f a
  (Child f) <*> (Root a) = Child $ f a
  (Child f) <*> (Child a) = Child $ f a

instance Monad Node where
  return = pure
  (Root a) >>= f = Root (f a)
--}



newtype Mesh a = Mesh { unMesh :: Tree a }
  deriving (Eq, Show, Generic, Functor, Applicative, Monad, Foldable, Traversable) 

data CommStats = CommStats
  { nothing :: ()
  }


data MeshState a = MeshState
  { structure :: Tree a
  }


messageRoute :: Mesh a -> a -> a -> [a]
messageRoute mesh source sink = undefined

connectionStrengths :: Mesh a -> [(a :* a)]
connectionStrengths = undefined

cpuLoadCheck :: Mesh a -> Int -> [a]
cpuLoadCheck = undefined
