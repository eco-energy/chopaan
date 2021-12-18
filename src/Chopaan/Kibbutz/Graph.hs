{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, StandaloneDeriving, UndecidableInstances, DerivingStrategies, DerivingVia, DeriveFunctor #-}
{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, QuantifiedConstraints, DataKinds, PolyKinds, KindSignatures, TypeOperators, FlexibleContexts #-}
module Chopaan.Kibbutz.Graph where

import Prelude hiding (id, (.), const, curry, uncurry)
import GHC.Generics
import Control.Newtype.Generics
import Control.DeepSeq
import ConCat.Misc
import ConCat.Category hiding (second)

import Data.Bifunctor
import Data.Functor.Const
import Data.Functor.Rep
import Data.Distributive
import Data.Monoid
import qualified Algebra.Graph.Labelled as G
import Algebra.Graph.Label

import Chopaan.Graph.G
import Chopaan.Node.HW
import Chopaan.Node.Mesh


class Arena a where
  data Pos a
  data Dis v a
  pos :: a -> Pos a
  dis :: a -> Dis (Pos a) a
  mkPos :: Pos a -> a
  mkDis :: Dis (Pos a) a -> a


--newtype instance MkArena i o = ArenaIO o (forall x. Const i x)

data ArenaIO i o = forall x. ArenaIO o (x -> i)

-- This Encoding does not work, because we cannot write an instance of Arena
-- for ArenaIO!
instance Arena (ArenaIO i o) where
  data Pos (ArenaIO i o) = PosIO
  data Dis (Pos (ArenaIO i o)) (ArenaIO i o) = DisIO 
  pos (ArenaIO o f)  = PosIO
  dis (ArenaIO o f) = DisIO

mkArenaIO :: i -> o -> ArenaIO i o
mkArenaIO i o = ArenaIO o (const i)

type Self s = ArenaIO s s

type Closed = ArenaIO () ()

type Motor s = ArenaIO () s

type Sensor s = ArenaIO s ()


data Lens domain codomain = Lens
  { observe :: Pos domain -> Pos codomain
  , interpret :: Pos domain
            -> Dis codomain (Pos codomain)
            -> Dis domain (Pos domain)
  }

type IdLens a = Lens a a

idLens :: IdLens a
idLens = Lens (id) (const id)

compLens :: Lens j k -> Lens i j -> Lens i k
compLens (Lens o' i') (Lens o i) = Lens obs interp
  where
    obs = o' . o
    interp p = (i p) . (i' (o p))
    
instance Category Lens where
  id = idLens
  (.) = compLens

newtype KbtzG n e a = KbtzG (G.Graph e (n, a))
  deriving (Generic, Generic1)
  deriving anyclass (Newtype, NFData)
  deriving (Functor) 
  deriving (Eq, Ord, Show) via (G.Graph e (n, a))

instance Bifunctor (KbtzG n) where
  bimap e' v' (KbtzG g) = KbtzG $ bimap e' (second v') g 

--instance Representable (KbtzG n e)

newtype HWG n r = HWG (KbtzG n (Distance r) (HW r))
  deriving (Generic)
  deriving anyclass Newtype

newtype PowerG n = PG (KbtzG n Watts WattHours)
  deriving (Generic)
  deriving anyclass Newtype

newtype MeshG n = MeshG (KbtzG n RxSignal MeshNode)
  deriving (Generic)
  deriving anyclass Newtype



-- $ x is existentialised so that any of the above graphs can be substituted in.
type Op e n a = forall x. ( O x ~ (G.Graph e (n, a)), Newtype x, Monoid e) => Binop x

ov :: (Monoid e) => (Op e n a)
ov = (inNew2) G.overlay

con :: e -> Op e n a
con e = inNew2 (G.connect e)


type TestG = KbtzG Int (Sum Int) Int 

test :: (TestG, TestG) -> Binop TestG -> TestG 
test (x, y) f = f x y  


test' :: Binop TestG 
test' = \x y -> (ov x $ con mempty x y)

