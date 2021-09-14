{-# language
  AllowAmbiguousTypes,
  DeriveGeneric,
  FlexibleContexts,
  FlexibleInstances,
  RankNTypes,
  TypeApplications,
  TypeInType,
  ScopedTypeVariables,
  TypeOperators,
  DefaultSignatures,
  UndecidableInstances
  #-}

-- https://stackoverflow.com/questions/27815489/is-it-possible-to-list-the-names-and-types-of-fields-in-a-record-data-type-that

module Data.Selectors where

import Type.Reflection
import GHC.Generics

class Selectors rep where
  selectors :: [(String, SomeTypeRep)]

instance Selectors f => Selectors (M1 D x f) where
  selectors = selectors @f

instance Selectors f => Selectors (M1 C x f) where
  selectors = selectors @f

instance (Selector s, Typeable t) => Selectors (M1 S s (K1 R t)) where
  selectors =
    [(selName (undefined :: M1 S s (K1 R t) ()) , SomeTypeRep (typeRep @t))]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
  selectors = selectors @a ++ selectors @b

instance Selectors U1 where
  selectors = []

selectorsRep :: forall a. (Selectors (Rep a)) => [(String, SomeTypeRep)]
selectorsRep = selectors @(Rep a)

instance (Selectors (Rep a)) => Selectors a where
  selectors = selectorsRep @a
