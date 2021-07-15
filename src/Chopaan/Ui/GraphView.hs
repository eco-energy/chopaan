{-# LANGUAGE FlexibleInstances, TypeFamilies, InstanceSigs
, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints, TemplateHaskell
, RankNTypes, FlexibleContexts, AllowAmbiguousTypes, ScopedTypeVariables, GADTs, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings, PackageImports, ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase, TypeOperators, TypeApplications, LiberalTypeSynonyms, CPP  #-}
{-# LANGUAGE IncoherentInstances, TupleSections #-}
module Chopaan.Ui.GraphView where

import Data.Aeson as A
import Data.Maybe
import Data.Text hiding (empty, zip, filter)
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding as T


import           GHCJS.DOM                               (currentDocumentUnchecked,
                                                          currentWindowUnchecked)
import "ghcjs-dom" GHCJS.DOM.Document                      (createElement)
import           GHCJS.DOM.Element                      (setId)
import           GHCJS.DOM.NonElementParentNode          (getElementById)

#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle
#else
import           Language.Javascript.JSaddle hiding (JSM, MonadJSM)
#endif

import           Shpadoinkle

import Shpadoinkle (Html(..), liftC, text, JSM, MonadJSM, Continuation, Html,
                     RawNode (..),
                     atomically, retrySTM, baked,
                     constUpdate, done,
                     kleisli, mapC, pur,
                     readTVarIO, text,
                     writeTVar)
import Shpadoinkle.Widgets.Types (Humanize(..), Present(..))
import Control.Monad.IO.Class
import qualified Shpadoinkle.Html as H
--import Shpadoinkle.Html.TH.AssetLink (assetLink)
import Shpadoinkle.Template.TH
import Shpadoinkle.Lens

import Chopaan.Graph.G as G
import qualified Chopaan.Ui.Style as Css
import Chopaan.Graph.Snapshot
import Chopaan.Ui.ThreeD (threeDM, grid3D)
import Data.FileEmbed

default (Text)

render3dGrid :: forall m n. (Applicative m, Eq n, Humanize n) => SG n -> Html m ()
render3dGrid sg = case sg of
  (G.Mesh (SG ms)) -> renderBaked ms 
  (G.Transactor (SG ms)) -> renderBaked ms 
  (G.Status (SG ms)) -> renderBaked ms
  (G.Flow (SG ms)) -> renderBaked ms
  where
    renderBaked :: forall v l.
                 (Eq l, Eq v, NFData l, NFData v, Humanize l, Humanize v)
               =>  SnapshotGraph n v l -> Html m ()
    renderBaked (ns, ls) = H.baked $ do
      (, retrySTM) <$> (threeDM objF  elements)
        where
          elements = fmap (fromJust) $ filter (isJust) $ _nodeAttributes <$> ns
          objF = grid3D 5 5 25


renderKbtzGraph :: forall m n. (Applicative m, Ord n, Humanize n) => SG n -> Html m () -- SG
renderKbtzGraph sg = case sg of
  (G.Mesh (SG ms)) -> renderThis ms -- (pimap meshEndo $) 
  (G.Transactor (SG ms)) -> renderThis ms -- pimap stakeEndo $ 
  (G.Status (SG ms)) -> renderThis ms -- pimap statusEndo $
  (G.Flow (SG ms)) -> renderThis ms -- pimap statusEndo $
  where
    renderThis :: forall v l.
      (Monoid l, Eq l, Ord v, Show l, Show v, Humanize l, Humanize v)
      =>  SnapshotGraph n v l -> Html m ()
    renderThis (ns, ls) = H.div_
      [ H.div nodeGridStyle [ nodeHtml n | n <- ns]
      , H.div edgeGridStyle $
          [ edgeHtml (_linkAttributes l) (_sourceNode l) (_destinationNode l) | l <- ls ]
      ]
      where
        elements = (fmap (fromJust) $ filter (isJust) $ _nodeAttributes <$> ns)
        nodeHtml :: SnapshotNode n v -> Html m ()
        nodeHtml n = H.div textBoxCSS
          $ (pure . H.text . humanize . _nodeId $ n)
          <> (fromMaybe mempty $ (present . show <$> (_nodeTimestamp n)))
          <> (fromMaybe mempty $ (pure . H.text . humanize) <$> (_nodeAttributes n))
        edgeHtml l n n' = H.div textBoxCSS [ H.text . humanize $ l ]
        textBoxCSS = [H.class' $ Css.flex
                    <> Css.flex_grow
                    <> Css.border_solid
                    <> Css.border_4
                    <> Css.border_blue_500
                  ]
        nodeGridStyle = [H.class' $ Css.grid <> Css.grid_flow_col <> Css.grid_cols_3 <> Css.gap_4 ]
        edgeGridStyle = [H.class' $ Css.grid <> Css.grid_flow_row <> Css.grid_cols_3 <> Css.gap_4 ]

renderWith3 :: (ToJSON n, ToJSON v, ToJSON e) => SnapshotGraph n v e -> Html m ()
renderWith3 a = baked $ do
  doc <- currentDocumentUnchecked
  (notify, stream) <- H.mkGlobalMailboxAfforded constUpdate
  isSubsequent <- traverse toJSVal =<< getElementById doc cont
  case isSubsequent of
    Just raw -> return (RawNode raw, stream)
    Nothing -> do
      container' <- createElement doc "div"
      setId container' cont
      eval $ T.decodeUtf8 thr
      eval $ T.decodeUtf8 thrCss
      eval $ T.decodeUtf8 thrCtrl
      eval $ T.decodeUtf8 tween
      eval $ T.decodeUtf8 gr
      --  \a' -> (liftIO . print $ a') >>
      liftIO $ print (A.toJSON a)
      -- jsg1 "initGrid" (A.toJSON a)
      c <- toJSVal container'
      return (RawNode c, stream)
  where
    cont :: Text
    cont = "gridConTrainer"
    gr = $(embedFile "js/gridRenderer.js")
    thr = $(embedFile "js/three.min.js")
    thrCss = $(embedFile "js/CSS3DRenderer.js")
    thrCtrl = $(embedFile "js/TrackballControls.js")
    tween = $(embedFile "js/Tween.js")

cview :: Html m a
cview = H.div [
  H.className "my-view"
  ] $(embedHtml "js/gridRenderer.html")

renderGrid :: forall m n. (MonadJSM m, ToJSON n) => SG n -> Html m ()
renderGrid sg = case sg of
  (G.Mesh (SG ms)) -> renderWith3 ms
  (G.Transactor (SG ms)) -> renderWith3 ms 
  (G.Status (SG ms)) -> renderWith3 ms
  (G.Flow (SG ms)) -> renderWith3 ms
