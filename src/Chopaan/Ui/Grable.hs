{-# LANGUAGE FlexibleInstances, TypeFamilies, InstanceSigs
, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints
, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE LambdaCase, TypeOperators, TypeApplications, TupleSections, CPP #-}
module Chopaan.Ui.Grable where

#ifndef ghcjs_HOST_OS
import ConCat.Misc (inNew2)
#else
import qualified Control.Category as C
#endif

--import Control.PseudoInverseCategory (pimap, EndoIso(..), PseudoInverseCategory(..))
import qualified Control.Newtype.Generics as N
import Control.DeepSeq (NFData)
import Data.Monoid (Sum(..))
import Data.Text (Text, pack)
import Data.Bifunctor
import Data.Aeson
import GHC.Generics (Generic, Generic1)

import Shpadoinkle.Backend.Snabbdom (runSnabbdom)

import Shpadoinkle (Html(..), liftC, text)
import Shpadoinkle.Run (runJSorWarp, simple)
import Shpadoinkle.Html (div_, getBody, Prop) --, input', onInput

import Shpadoinkle.Widgets.Types.Core
import qualified Shpadoinkle.Widgets.Table as T


import Algebra.Graph.Labelled


data Op = Op1 | Op2 | Op3 | Op4 | Op5
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, NFData)

newtype AnOp = AnOp { unOp :: Op }
  deriving (Eq, Ord, Show, Read, Bounded, Generic)
  deriving newtype (NFData)

instance Enum AnOp where
  toEnum = AnOp . toEnum
  fromEnum (AnOp o) = fromEnum o 

deriving newtype instance Enum (Sum Int)
deriving newtype instance Real (Sum Int)
deriving newtype instance Integral (Sum Int)


newtype S = S { unS :: Sum Int }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Num, NFData, Semigroup, Monoid)

instance Show S where
  show = show . getS

deriving newtype instance Enum S
deriving newtype instance Real S
deriving newtype instance Integral S

getS :: S -> Int
getS = getSum . unS

inS :: (Int -> Int) -> (S -> S)
inS f = S . Sum . f . getS 

inS2 :: (Int -> Int -> Int) -> (S -> S -> S)
inS2 f = \s s' -> S . Sum . (uncurry f) $ (getS s, getS s')

instance N.Newtype S


newtype Gr flow state = Gr { unGr :: (Graph flow state) }
  deriving stock (Eq, Ord, Show, Generic, Generic1)
  deriving newtype (Num, NFData, Functor, Bifunctor)


deriving instance Generic1 (Graph flow)


emptyGr :: (GrConn flow state) => Gr flow state
emptyGr = Gr empty

grProps :: [(Text, Prop m (Gr flow state))]
grProps = []

grRow :: (Show a, Show b, Show c) => (a, b, c) -> Text
grRow (l, e, e') = (pack . show $ l)

instance N.Newtype (Gr flow state)

-- $ Shpadoinkle Instances
instance (Show state, Show flow) => Humanize (Gr flow state)


instance (GrConn flow state) => T.Tabular (Gr flow state) where
  type Effect (Gr flow state) m = Monad m
  toRows :: Gr flow state -> [T.Row (Gr flow state)]
  toRows (Gr g) = Row <$> edgeList g
  toCell :: Functor m
    => T.Effect (Gr flow state) m
    => (Gr flow state)
    -> T.Row (Gr flow state) -> T.Column (Gr flow state) -> [Html m (Gr flow state)] 
  toCell (Gr g) (Row _) (Column col) =
    let --c = context ((==) col) gr
        es = edgeList g
        grHtml :: Monad m => [Html m (Gr flow state)]
        grHtml = undefined <$> es
          where
            -- xs :: Monad m => (flow, state, state) -> Html m (Gr flow state)
            -- xs e = pimap iso (text $ grRow e)
            -- iso :: EndoIso [(flow, state, state)] (Gr flow state)
            -- iso = EndoIso id isoL isoR
            --   where
            --     isoL :: [(flow, state, state)] -> Gr flow state
            --     isoL xs' = foldl ov emptyGr (fmap e' xs')
            --     isoR :: Gr flow state -> [(flow, state, state)]
            --     isoR = edgeList . unGr
            --     e' (x, y, z) = Gr $ edge x y z
                
    in grHtml  
  sortTable ::
    T.SortCol (Gr flow state)
    -> T.Row (Gr flow state)
    -> T.Row (Gr flow state) -> Ordering
  sortTable (T.SortCol (Column _) s) (Row a) (Row b) = case s of
    T.ASC -> compare a b
    T.DESC -> compare b a

-- $ Data Instance and their Instances

data instance T.Row (Gr f s) = Row (f, s, s)
data instance T.Column (Gr f s) = Column (Tag s f)

instance (Show s, Show f) => Show (T.Column (Gr f s)) where
  show (Column a) = show a
  
instance (Show s, Show f) => Humanize (T.Column (Gr f s))

instance (Bounded s) => Bounded (T.Column (Gr f s)) where
  minBound = Column minBound
  maxBound = Column maxBound

deriving instance (Eq s, Eq f) => Eq (T.Column (Gr f s))
  
deriving instance (Ord s, Ord f) => Ord (T.Column (Gr f s))

data Tag v e = Title | Vx | Lx
  deriving (Eq, Ord, Enum, Bounded, Show, Generic, ToJSON, FromJSON, NFData)


instance (Enum s) => Enum (T.Column (Gr f s)) where
  toEnum :: Int -> T.Column (Gr f s)
  toEnum = Column . toEnum @(Tag s f)
  fromEnum :: T.Column (Gr f s) -> Int
  fromEnum (Column s) = fromEnum s



-- $ Graph View
-- $ Constraints for edge labels and nodes
type GrConn f s = (Bounded s, Show s, Ord s, Eq s, Enum s, Show f, Monoid f, Ord f)

-- $ Constraints for edge labels and nodes, along with monad constraints
type GrConnM m f s = (Monad m, GrConn f s)



#ifdef ghcjs_HOST_OS
(<~) :: (C.Category k)
     => (b `k` b') -> (a' `k` a) -> ((a `k` b) -> (a' `k` b'))
(h <~ f) g = h C.. g C.. f

inNew :: (N.Newtype p, N.Newtype q) =>
         (N.O p -> N.O q) -> (p -> q)
inNew = N.pack <~ N.unpack
{-# INLINE inNew #-}

inNew2 :: (N.Newtype p, N.Newtype q, N.Newtype r) =>
          (N.O p -> N.O q -> N.O r) -> (p -> q -> r)
inNew2 = inNew <~ N.unpack
{-# INLINE inNew2 #-}
#endif

-- $ Overlay two Gr
ov :: (GrConn f s) => Gr f s -> Gr f s -> Gr f s
ov = inNew2 overlay

-- Fold over a collection of graphs to get a graph
mergeGr :: (GrConn f s, Foldable t) => t (Gr f s) -> Gr f s 
mergeGr = foldl ov emptyGr

-- $ render a graph to html
grView :: forall m f s. (GrConnM m f s)
  => Gr f s
  -> T.SortCol (Gr f s)
  -> Html m (Gr f s, (T.SortCol (Gr f s)))
grView = T.view @m

data Model l n = Model
  { graph :: Gr l n }
  deriving (Eq, Show, Generic)
deriving instance (Generic s, Generic o, NFData s, NFData o) => NFData (Model s o)


-- asTitle :: forall f s. (GrConn f s) => EndoIso (Gr f s) (Gr f s, T.SortCol (Gr f s))
-- asTitle = EndoIso id (\a -> (a, sortColGr (const Title) a)) fst

-- asTitle' :: forall f s. (GrConn f s) => EndoIso (Gr f s, T.SortCol (Gr f s)) (Gr f s) 
-- asTitle' = piinverse asTitle

-- asVx :: forall f s. (GrConn f s) => EndoIso (Gr f s) (Gr f s, T.SortCol (Gr f s))
-- asVx = EndoIso id (\a -> (a, sortColGr (const Vx) a)) fst

-- asVx' :: forall f s. (GrConn f s) => EndoIso (Gr f s, T.SortCol (Gr f s)) (Gr f s) 
-- asVx' = piinverse asVx

-- asLx :: forall f s. (GrConn f s) => EndoIso (Gr f s) (Gr f s, T.SortCol (Gr f s))
-- asLx = EndoIso id (\a -> (a, sortColGr (const Lx) a)) fst

-- asLx' :: forall f s. (GrConn f s) => EndoIso (Gr f s, T.SortCol (Gr f s)) (Gr f s) 
-- asLx' = piinverse asLx


-- -- $ The view takes continuations as arguments

-- view :: (Monad m) => Model S Op -> Html m (Model S Op)
-- view model = div_
--   [ liftC (\g' m -> m {graph = g'}) graph $ pimap asTitle' (grView (graph model) (sortCol model)) ]


-- $ TODO: head here after vertex list doesn't make any sense.
-- $       should the column be a list?
-- $       I can make it a subgraph
sortColGr :: (GrConn f s) => (s -> Tag s f) -> Gr f s -> T.SortCol (Gr f s)
sortColGr f = (flip T.SortCol T.ASC) . (Column . f . head . vertexList . unGr) 

sortCol :: (GrConn f s) => Model f s -> T.SortCol (Gr f s)
sortCol m = sortColGr (const Title) (graph m)

main :: IO ()
main = runJSorWarp 8080 $
  simple runSnabbdom (Model (mergeGr $ (fmap snd) (iterate opS (0, emptyGr)))) undefined getBody




gr :: f -> s -> s -> Gr f s
gr l x y = N.pack $ edge l x y 

opS :: (S, Gr S Op) -> (S, Gr S Op)
opS (i, g) = (inc i, ov g $ gr (inc i) (op i) (op (inc i)))
  where
    op :: S -> Op
    op i' = unOp . toEnum $ rem (getS i') (fromEnum $ maxBound @Op)
    inc = (+1)
