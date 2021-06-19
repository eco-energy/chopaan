{-# LANGUAGE FlexibleInstances, TypeFamilies, InstanceSigs
, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints
, RankNTypes, FlexibleContexts, AllowAmbiguousTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase, TypeOperators, TypeApplications #-}
module Chopaan.Ui.GraphView where

import ConCat.Misc (inNew, inNew2, (:*), (:+))
import GHC.Generics (Generic, Generic1)
import qualified Control.Newtype.Generics as N
import Control.DeepSeq (NFData)
import Control.PseudoInverseCategory
import Data.Aeson
import Data.Text hiding (empty, zip)
import Data.Text.Lazy (toStrict)
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

import qualified Algebra.Graph.Labelled as AG
import qualified Algebra.Graph as G
--import Algebra.Graph.Label

import Diagrams.Prelude
import qualified Diagrams.Envelope as E
import Diagrams.Backend.SVG (B)
import qualified Diagrams.TwoD.Text as DT
import Diagrams.TwoD.Layout.Grid
import Graphics.SVGFonts
import qualified Clay as C

import NetSpider.Snapshot

import Chopaan.Node.Mesh
import Chopaan.Node.Folds
import Chopaan.Kibbutz.Transactor
import Chopaan.Graph
import Chopaan.Graph.G as G
import Chopaan.Node.NodeId
import qualified Chopaan.Ui.Style as Css 



newtype Pos = Pos Double
  deriving stock (Generic)
  deriving newtype (Fractional, Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present, NFData)
  deriving (Semigroup, Monoid) via (Sum Double)



-- meshEndo :: EndoIso (SnapshotGraph NodeMAC MeshNode RxSignal) (SG NodeMAC)
-- meshEndo = EndoIso id fwd back
--   where
--     fwd = MeshG
--     back (MeshG a) = a

-- stakeEndo :: EndoIso (SnapshotGraph NodeMAC SensorR Stake) (SG NodeMAC)
-- stakeEndo = EndoIso id fwd back
--   where
--     fwd = StakeG
--     back (StakeG a) = a

-- statusEndo :: EndoIso (SnapshotGraph NodeMAC SensorR TxStatus) (SG NodeMAC)
-- statusEndo = EndoIso id fwd back
--   where
--     fwd = StatusG
--     back (StatusG a) = a


renderKbtzGraph :: forall m n. (Applicative m, Ord n, Show n) => SG n -> Html m () -- SG
renderKbtzGraph sg = case sg of
  (G.Mesh (SG ms)) -> renderThis ms -- (pimap meshEndo $) 
  (G.Transactor (SG ms)) -> renderThis ms -- pimap stakeEndo $ 
  (G.Status (SG ms)) -> renderThis ms -- pimap statusEndo $
  (G.Flow (SG ms)) -> renderThis ms -- pimap statusEndo $
  where
    renderThis :: forall n v l.
      (Monoid l, Eq l, Ord v, Show l, Show v, Ord n, Show n)
      =>  SnapshotGraph n v l -> Html m ()--(SnapshotGraph n v l)
    renderThis (ns, ls) = H.div
      [ H.class' $ Css.flex <> Css.flex_grow]
      (
        [ H.div (nodeClasses i) $ [ nodeHtml n ] | (i, n) <- zip [0,(1 :: Double)..] $ ns]
        <>
        [ H.div (edgeClasses i) $ [ edgeHtml l v v' ]
        | (i, (l, (n, v), (n', v'))) <- zip [(0 :: Double), 1..] $ castLinks (ns, ls)
        ]
      )
      where
        grNameC i = H.class' $ "graph-" <> (pack . show $ i) 
        posCss = H.class' . toStrict . C.render . C.position $ C.static
        nodeClasses i = [grNameC i, posCss]
        edgeClasses i = [grNameC i,  posCss]
        nodeHtml = H.text . pack . show
        edgeHtml l v v' = H.div_ [ H.text . pack . show $ v
                                 , H.text . pack . show $ v'
                                 , H.text . pack . show $ l ]
