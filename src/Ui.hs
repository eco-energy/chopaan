{-# LANGUAGE FlexibleInstances, TypeFamilies, InstanceSigs
, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints
, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE LambdaCase, TypeOperators, TypeApplications, TupleSections #-}
module Ui where

import ConCat.Misc (inNew2)
import Control.PseudoInverseCategory (pimap, EndoIso(..), PseudoInverseCategory(..))
import qualified Control.Newtype.Generics as N
import Control.DeepSeq (NFData)
import Data.Monoid (Sum(..))
import Data.Text (Text, pack)
import Data.Bifunctor
import GHC.Generics (Generic, Generic1)

import Shpadoinkle.Backend.ParDiff (runParDiff)

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
        grHtml = xs <$> es
          where
            xs :: Monad m => (flow, state, state) -> Html m (Gr flow state)
            xs e = pimap iso (text $ grRow e)
            iso :: EndoIso [(flow, state, state)] (Gr flow state)
            iso = EndoIso id isoL isoR
              where
                isoL :: [(flow, state, state)] -> Gr flow state
                isoL xs' = foldl ov emptyGr (fmap e' xs')
                isoR :: Gr flow state -> [(flow, state, state)]
                isoR = edgeList . unGr
                e' (x, y, z) = Gr $ edge x y z
                
    in grHtml  
  sortTable ::
    T.SortCol (Gr flow state)
    -> T.Row (Gr flow state)
    -> T.Row (Gr flow state) -> Ordering
  sortTable (T.SortCol (Column _) s) (Row a) (Row b) = case s of
    T.ASC -> compare a b
    T.DESC -> compare b a

-- $ Data Instance and their Instances

data instance T.Row (Gr flow state) = Row (flow, state, state)
data instance T.Column (Gr flow state) = Column state

instance (Show state, Show flow) => Show (T.Column (Gr flow state)) where
  show (Column a) = show a
  
instance (Show state, Show flow) => Humanize (T.Column (Gr flow state))

instance (Bounded state) => Bounded (T.Column (Gr flow state)) where
  minBound = Column minBound
  maxBound = Column maxBound

deriving instance (Eq state, Eq flow) => Eq (T.Column (Gr flow state))
  
deriving instance (Ord state, Ord flow) => Ord (T.Column (Gr flow state))
  
instance (Enum state) => Enum (T.Column (Gr flow state)) where
  toEnum :: Int -> T.Column (Gr flow state)
  toEnum = Column . toEnum
  fromEnum :: T.Column (Gr flow state) -> Int
  fromEnum (Column s) = fromEnum s



-- $ Graph View
-- $ Constraints for edge labels and nodes
type GrConn f s = (Bounded s, Show s, Ord s, Eq s, Enum s, Show f, Monoid f, Ord f)

-- $ Constraints for edge labels and nodes, along with monad constraints
type GrConnM m f s = (Monad m, GrConn f s)

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


isoSort :: forall f s. (GrConn f s) => EndoIso (Gr f s) (Gr f s, T.SortCol (Gr f s))
isoSort = EndoIso id (\a -> (a, sortColGr a)) fst

isoSort' :: forall f s. (GrConn f s) => EndoIso (Gr f s, T.SortCol (Gr f s)) (Gr f s) 
isoSort' = piinverse isoSort

-- $ The view takes continuations as arguments

view :: (Monad m) => Model S Op -> Html m (Model S Op)
view model = div_
  [ liftC (\g' m -> m {graph = g'}) graph $ pimap isoSort' (grView (graph model) (sortCol model)) ]


-- $ TODO: head here after vertex list doesn't make any sense.
-- $       should the column be a list?
-- $       I can make it a subgraph
sortColGr :: (GrConn f s) => Gr f s -> T.SortCol (Gr f s)
sortColGr = (flip T.SortCol T.ASC) . (Column . head . vertexList . unGr) 

sortCol :: (GrConn f s) => Model f s -> T.SortCol (Gr f s)
sortCol = sortColGr . graph 

main :: IO ()
main = runJSorWarp 8080 $
  simple runParDiff (Model (mergeGr $ (fmap snd) (iterate opS (0, emptyGr)))) view getBody




gr :: f -> s -> s -> Gr f s
gr l x y = N.pack $ edge l x y 

opS :: (S, Gr S Op) -> (S, Gr S Op)
opS (i, g) = (inc i, ov g $ gr (inc i) (op i) (op (inc i)))
  where
    op :: S -> Op
    op i' = unOp . toEnum $ rem (getS i') (fromEnum $ maxBound @Op)
    inc = (+1)
