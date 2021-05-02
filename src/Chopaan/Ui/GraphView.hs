{-# LANGUAGE FlexibleInstances, TypeFamilies, InstanceSigs
, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints
, RankNTypes, FlexibleContexts, AllowAmbiguousTypes, ScopedTypeVariables #-}
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

import Chopaan.Kibbutz.Mesh
import Chopaan.Node.Folds
import Chopaan.Kibbutz.Transactor
import Chopaan.Kibbutz.Kibbutz
import Chopaan.Graph
import Chopaan.Node.NodeId
import NetSpider.Graph
import NetSpider.Snapshot

data G l v = N v | E l v v

newtype Pos = Pos Double
  deriving stock (Generic)
  deriving newtype (Fractional, Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present, NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

layout :: forall m l v a. (Monoid l, Monoid v) => Gr l v -> (Gr l v -> Text) -> (Gr l v -> [Html m a]) -> Html m a
layout gr style mk =  H.div [H.class' (style gr)] $ mk gr -- 



meshEndo :: EndoIso (SnapshotGraph NodeMAC MeshNode RxSignal) SG
meshEndo = EndoIso id fwd back
  where
    fwd = MeshSnapshot
    back (MeshSnapshot a) = a

stakeEndo :: EndoIso (SnapshotGraph NodeMAC SensorS Stake) SG
stakeEndo = EndoIso id fwd back
  where
    fwd = StakeSnapshot
    back (StakeSnapshot a) = a

statusEndo :: EndoIso (SnapshotGraph NodeMAC SensorS TransactionStatus) SG
statusEndo = EndoIso id fwd back
  where
    fwd = StatusSnapshot
    back (StatusSnapshot a) = a

renderKbtzGraph :: forall m.(Applicative m) => SG -> Html m SG
renderKbtzGraph sg = case sg of
  (MeshSnapshot ms) -> pimap meshEndo $ renderM ms
  (StakeSnapshot ms) -> pimap stakeEndo $ renderSk ms
  (StatusSnapshot ms) -> pimap statusEndo $ renderSt ms
  where
    renderM = renderThis @NodeMAC @MeshNode @RxSignal
    renderSk = renderThis @NodeMAC @SensorS @Stake 
    renderSt = renderThis @NodeMAC @SensorS @TransactionStatus
    renderThis :: forall n v l.
      (Monoid l, Eq l, Ord v, Show l, Show v, Ord n, Show n)
      =>  SnapshotGraph n v l -> Html m (SnapshotGraph n v l)
    renderThis (ns, ls) = H.div [] $ [
      H.div (nodeClasses i) $ [ nodeHtml n ]
      | (i, n) <- zip [0,(1 :: Double)..] $ ns
      ] <> [
      H.div (edgeClasses i) $ [ edgeHtml l v v' ]
      | (i, (l, (n, v), (n', v'))) <- zip [(0 :: Double), 1..] $ castLinks (ns, ls)
      ]
      where
        grNameC i = H.class' $ "graph-" <> (pack . show $ i) 
        posCss = H.class' . toStrict . C.render . C.position $ C.static
        nodeClasses i = [grNameC i, posCss]
        edgeClasses i = [grNameC i,  posCss]
        nodeHtml = H.text . pack . show
        edgeHtml l v v' = H.div_ [ H.text . pack . show $ v
                                 , H.text . pack . show $ v'
                                 , H.text . pack . show $ l ]
