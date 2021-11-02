{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE DataKinds, OverloadedLabels, ConstraintKinds, MultiParamTypeClasses, TypeFamilies, AllowAmbiguousTypes #-}
module Chopaan.Graph.Kbtz.Types where

import Prelude hiding ((.), id)
import GHC.Generics
import Control.Category
import Data.Generic.HKD
import Data.Functor.Barbie

import qualified Data.Text as T
import Data.Monoid hiding (Product)
import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor.Product


import Data.Greskell.GraphSON (FromGraphSON(..), GValue)
import Data.Greskell.Graph (Key(..), Element, KeyValue(..))
import Data.Greskell.GTraversal (Walk, SideEffect, gAddV)
import Data.Greskell.Binder (Binder, newBind)
import Data.Greskell.PMap
  ( PMap, Multi, PMapLookupException,
    lookupAs
  )
import Data.Greskell.Extra (writeKeyValues, (<=:>))
import Data.Aeson (ToJSON)


type Optional a = HKD a Maybe
type Partial a = HKD a  Last          -- Fields may be missing.
type Bare    a = HKD a  Identity      -- All must be present.
type Labels  a = HKD a (Const String) -- Every field holds a string.
type Gong a = HKD a (Either PMapLookupException)


type ElementIso inner result a = (Generic a
                                 , Label a
                                 , ApplicativeB (HKD a)
                                 , TraversableB (HKD a)
                                 , ConstraintsB (HKD a)
                                 , AllB inner (HKD a)
                                 , Construct result a
                                 )

type ParseEl a = (ElementIso FromGraphSON (Either PMapLookupException) a)


parseByLabels ::
  forall a. (Generic a, ParseEl a)
  => Labels a -> PMap Multi GValue -> Gong a
parseByLabels l pm = bmapC @FromGraphSON  psqs l
  where
    psqs :: forall v. (FromGraphSON v) => (Const String v) -> (Either PMapLookupException v)
    psqs = (flip lookupAs pm) . Key . T.pack . getConst

parse' :: forall a. (ParseEl a) => PMap Multi GValue -> Either PMapLookupException a
parse' = construct . parseByLabels (label @a)


class (Element v) => HasV v a where
  data VId a
  vIdKey :: Key v (VId a)
  vLabel :: T.Text
  vProps :: a -> Binder (Walk SideEffect v v)
  
class HasE e a where

type EncodeEl a = ElementIso ToJSON Identity a

-- $ Encode a vertex by extracting it's id and encoding everything else as a winery ting
encode' :: forall v a.
  (Element v, HasV v a, EncodeEl a)
  => a -> Binder (Walk SideEffect v v)  
encode' = fmap writeKeyValues . toKeyValues'
  where
    toKeyValues' :: a -> Binder [KeyValue v]
    toKeyValues' a = sequence $ bfoldMapC @ToJSON toKV
      $ bzip (label @a) (deconstruct @Identity a)
      where
        toKV :: forall x. (ToJSON x) =>
          ((Const String) `Product` Identity) x -> [Binder (KeyValue v)]
        toKV (Pair (Const s) (Identity v)) =  pure ((Key . T.pack $ s) <=:> v)


addV :: forall v a. (HasV v a, EncodeEl a) => a -> Binder (Walk SideEffect () v)
addV a = gAddV <$> (\x -> (newBind (vLabel @v @a)) >>> x) <*> (encode' a) 
