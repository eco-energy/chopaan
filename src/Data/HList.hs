{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, RankNTypes #-}
module Data.HList where

data HList :: [*] -> * where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

infixr 5 %:
(%:) :: x -> HList xs -> HList (x ': xs)
(%:) = HCons


class Apply f a b where
  apply :: f -> a -> b

class MapH f xs ys where
  mapH :: f -> HList xs -> HList ys

instance MapH f '[] '[] where
  mapH _ _ = HNil

instance (Apply f x y, MapH f xs ys) => MapH f (x ': xs) (y ': ys) where
  mapH f (HCons x' xs) = apply f x' %: mapH f xs

class FoldrH f acc xs where
  foldrH :: f -> acc -> HList xs -> acc

instance FoldrH f acc '[] where
  foldrH _ acc _ = acc

instance (Apply f x (acc -> acc), FoldrH f acc xs) => FoldrH f acc (x ': xs) where
  foldrH f acc (HCons x xs) = apply f x $ foldrH f acc xs

class HasThing a where
  getThing :: a -> Int

data Concat = Concat

instance (Semigroup a) => Apply Concat a (a -> a) where
  apply _ = (<>)

hToList :: HList '[[Int], [Int], [Int]] -> [Int]
hToList l = foldrH Concat [] l
