{-# LANGUAGE FlexibleInstances, TypeFamilies, InstanceSigs
, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints
, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase, TypeOperators, TypeApplications #-}
module Chopaan.Ui.GraphView where

import ConCat.Misc (inNew, inNew2, (:*), (:+))
import GHC.Generics (Generic, Generic1)
import qualified Control.Newtype.Generics as N
import Control.DeepSeq (NFData)

import Data.Text hiding (empty)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Bifunctor
import Data.Typeable

import Shpadoinkle (Html(..), liftC, text)
import Shpadoinkle.Run (runJSorWarp, simple)
import Shpadoinkle.Html (div_, getBody, input', onInput
                        , onOption, option, select, value, Prop(..))
import qualified Shpadoinkle.Html as H
import Shpadoinkle.Widgets.Types.Core

--import Algebra.Graph.Labelled
--import Algebra.Graph.Label

import Diagrams.Prelude
import qualified Diagrams.Envelope as E
import Diagrams.Backend.SVG (B)
import qualified Diagrams.TwoD.Text as DT
import Diagrams.TwoD.Layout.Grid
import Graphics.SVGFonts

import Chopaan.Kibbutz.Kibbutz
import Chopaan.Graph



-- $ Constraints for edge labels and nodes
type GrConn f s = (Bounded s, Show s, Ord s, Eq s, Enum s, Show f, Monoid f, Ord f)

-- $ Constraints for edge labels and nodes, along with monad constraints
type GrConnM m f s = (Monad m, GrConn f s)

deriving instance Generic1 (Graph flow)

newtype Gr flow state = Gr { unGr :: (Graph flow state) }
  deriving stock (Eq, Ord, Show, Generic, Generic1)
  deriving newtype (Num, Functor, Bifunctor)


emptyGr :: (GrConn flow state) => Gr flow state
emptyGr = Gr empty

grProps :: [(Text, Prop m (Gr flow state))]
grProps = []

grEdge :: (Show a, Show b, Show c) => (a, b, c) -> Text
grEdge (l, e, e') = (pack . show $ l)

instance N.Newtype (Gr flow state)

-- $ Shpadoinkle Instances
instance (Show state, Show flow) => Humanize (Gr flow state)


newtype GrNode = GrNode Int deriving (Eq, Ord, Typeable, Show, Num)




graphView :: forall m flow state. (Applicative m, GrConn flow state)
  => Gr flow state -> Html m (Gr flow state)
graphView = (H.div grProps) . renderGraph' --render' $ graph
  where
    renderGraph' :: _
    renderGraph' = undefined
{--
  where
    render' :: Graph flow (GrNode, state) -> (GrNode, Diagram B) 
    render' = foldg mempty renderNode renderEdge
    renderNode :: (GrNode, state) -> (GrNode, Diagram B)
    renderNode (n, s) = let
      t :: Diagram B
      t = text' s
      r = (E.radius (V2 0 0) t)
      in (n, (circle r `atop` t) # named @GrNode n)
    renderEdge :: flow -> (GrNode, Diagram B) -> (GrNode, Diagram B) -> (GrNode, Diagram B)
    renderEdge _ (x, n) (y, n') = (y, connectOutside x y $ gridCat [n, n']) -- $ \[xn, yn] ->
      --atop (boundaryFrom xn unit_Y ~~ boundaryFrom yn unitY))
    text' :: Show s => s -> Diagram B
    text' s = stroke $ textSVG (show s) 1

--}
{--

renderKbtz :: forall t m n a. (KbtzConn t m n, IsName n, Show a, Monad (t m))
  => Kbtz t m n a
  -> t m (Diagram B)
renderKbtz = (fmap (gridCat . elems . (mapWithKey renderNode))) . kbtzState
  where
    renderNode :: n -> a -> Diagram B
    renderNode n s = let
      t :: Diagram B
      t = text' s
      r = E.radius (V2 0 0) t
      in (circle r `atop` t) # named n
    text' :: Show s => s -> Diagram B
    text' s = stroke $ textSVG (show s) 1
--}
