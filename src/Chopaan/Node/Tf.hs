{-# LANGUAGE DeriveGeneric, KindSignatures, FlexibleContexts, DeriveFunctor, ConstraintKinds, TypeApplications, InstanceSigs, ExplicitForAll, ScopedTypeVariables #-}
module Chopaan.Node.Tf where

import Control.Monad.Identity
import Control.Comonad.Identity
-- Sensor Transformations
import GHC.Generics

import Streamly
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

import qualified Streamly.Prelude as S

import ConCat.FFT
import ConCat.Scan

import ConCat.Sized
import ConCat.Shaped

import Data.Complex
import Data.Pointed
import Data.Key


newtype Sx (t :: (* -> *) -> * -> *) m a = Sx { unSx :: t m a } deriving (Generic, Generic1, Functor)

type SxConn t m = (IsStream t, MonadAsync m)

instance (SxConn t m) => Semigroup (Sx t m a) where
  (Sx s) <> (Sx s') = Sx $ s <> s'

instance (SxConn t m) => Monoid (Sx t m a) where
  mempty = Sx $ mempty

instance (SxConn t m, Comonad m) => Foldable (Sx t m) where
  foldr :: forall a b. (a -> b -> b) -> b -> Sx t m a -> b
  foldr f i (Sx a) = extract $ S.foldr f i (adapt $ a)

instance SxConn t m => Applicative (Sx t m) where
  pure = pure
  (Sx fs) <*> (Sx as) = Sx $ fs <*> as 

instance SxConn t m => Pointed (Sx t m) where
  point = pure

instance SxConn t m => Zip (Sx t m) where
  zipWith f (Sx a) (Sx a') = Sx (S.zipWith f a a') 

instance Sized (Sx t m) where
  size = 1024

{--
instance (SxConn t m, Comonad m, Traversable (t m)) => Traversable (Sx t m) where
  traverse :: forall f a b. (Applicative f) => (a -> f b) -> Sx t m a -> f (Sx t m b) 
  traverse f (Sx x) = undefined
    where
      s' :: f (Sx t m b)
      s' =  fs)
      fs :: t m (f b)
      fs = (f <$> x)  
--}

instance (IsStream t, MonadAsync m) => LScan (Sx t m) where
  lscan f = (f, mempty)

toC :: (SxConn t m, RealFloat a) => Sx t m a -> Sx t m (Complex a)
toC = fmap cis

x :: forall t m a. (SxConn t m, RealFloat a, Comonad m) => Sx t m a -> Sx t m (Complex a)
x = dft . toC
-- These are the required instances


--sX :: forall t m a. (SxConn t m, RealFloat a, Comonad m) => t m a -> t m (Sx t m a) 
--sX = (fmap Sx) . (S.chunksOf (size @(Sx t m)) idFold)


idFold :: (Monad m, Monoid a) => FL.Fold m a a
idFold = liftF mempty (\_ a -> a)

liftF :: (Monad m) => b -> (b -> a -> b) -> FL.Fold m a b
liftF b f = FL.Fold (\a b -> pure . FL.Partial $ f a b) (pure b) pure
