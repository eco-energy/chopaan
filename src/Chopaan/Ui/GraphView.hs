{-# LANGUAGE FlexibleInstances, TypeFamilies, InstanceSigs
, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints, TemplateHaskell
, RankNTypes, FlexibleContexts, AllowAmbiguousTypes, ScopedTypeVariables, GADTs, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings, PackageImports, ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase, TypeOperators, TypeApplications, LiberalTypeSynonyms, CPP  #-}
module Chopaan.Ui.GraphView where

import Data.Aeson as A
import Data.Text hiding (empty, zip)
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding as T


import           GHCJS.DOM                               (currentDocumentUnchecked,
                                                          currentWindowUnchecked)
import "ghcjs-dom" GHCJS.DOM.Document                      (createElement)
import           GHCJS.DOM.Element                       (setId)
import           GHCJS.DOM.NonElementParentNode          (getElementById)
import           GHCJS.DOM.RequestAnimationFrameCallback (RequestAnimationFrameCallback,
                                                          newRequestAnimationFrameCallback)
import           GHCJS.DOM.Window                        (Window,
                                                          requestAnimationFrame,
                                                          getInnerHeight,
                                                          getInnerWidth)

#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle
#else
import           Language.Javascript.JSaddle hiding (JSM, MonadJSM)
#endif

import           Shpadoinkle
import           UnliftIO.Concurrent                     (forkIO, threadDelay)


import Shpadoinkle (Html(..), liftC, text, JSM, MonadJSM, Continuation, Html,
                     RawNode (..),
                     atomically, baked,
                     constUpdate, done,
                     kleisli, mapC, pur,
                     readTVarIO, text,
                     writeTVar)
import Control.Monad.IO.Class
import Shpadoinkle.Run (runJSorWarp, simple)
import Shpadoinkle.Html (div_, getBody, input', onInput
                        , onOption, option, select, value, Prop(..))
import qualified Shpadoinkle.Html as H
import Shpadoinkle.Widgets.Types.Core


import qualified Clay as C


import Shpadoinkle.Template.TH


import Chopaan.Graph.G as G
import qualified Chopaan.Ui.Style as Css
import Chopaan.Graph.Snapshot
import Chopaan.CRUD
import Data.FileEmbed

default (Text)


renderKbtzGraph :: forall m n. (Applicative m, Ord n, Show n) => SG n -> Html m () -- SG
renderKbtzGraph sg = case sg of
  (G.Mesh (SG ms)) -> renderThis ms -- (pimap meshEndo $) 
  (G.Transactor (SG ms)) -> renderThis ms -- pimap stakeEndo $ 
  (G.Status (SG ms)) -> renderThis ms -- pimap statusEndo $
  (G.Flow (SG ms)) -> renderThis ms -- pimap statusEndo $
  where
    renderThis :: forall v l.
      (Monoid l, Eq l, Ord v, Show l, Show v)
      =>  SnapshotGraph n v l -> Html m ()--(SnapshotGraph n v l)
    renderThis (ns, ls) = H.div
      [ H.class' $ Css.flex <> Css.flex_grow]
      (
        [ H.div_ $ [ nodeHtml n ] | n <- ns]
        <>
        [ H.div_ $
          [ edgeHtml (_linkAttributes l) (_sourceNode l) (_destinationNode l)] | l <- ls ]
        <> [ cview ]
      )
      where
        grNameC i = H.class' $ "graph-" <> (pack . show $ i)
        posCss = H.class' . toStrict . C.render . C.position $ C.static
        nodeClasses i = [grNameC i, posCss]
        edgeClasses i = [grNameC i,  posCss]
        nodeHtml = H.text . pack . show
        edgeHtml l n n' = H.div_ [ H.text . pack . show $ n
                                 , H.text . pack . show $ l
                                 , H.text . pack . show $ n' ]


createGridObject :: JSVal -> JSVal -> JSM (JSVal)
createGridObject = undefined


renderWith3 :: (ToJSON n, ToJSON v, ToJSON e) => SnapshotGraph n v e -> Html m ()
renderWith3 a = cview
  where
    b = baked $ do
      --   $(embedHtml "src/Chopaan/Ui/gridRenderer.html")
      (notify, stream) <- H.mkGlobalMailboxAfforded constUpdate
      let file = $(embedFile "js/gridRenderer.js")
      liftIO . print $ file
      doc' <- currentDocumentUnchecked
      container' <- toJSVal =<< createElement doc' "div"
      eval $ T.decodeUtf8 file 
      --  \a' -> (liftIO . print $ a') >>  
      --jsg1 "initGrid" (A.toJSON a)
      return (RawNode container', stream)


cview :: Html m a
cview = H.div [
  H.className "my-view"
  ] $(embedHtml "js/gridRenderer.html")

--renderWithI = template id 

-- $ Why Can't I Write This?
-- onG :: Unop (forall v e. SnapshotGraph n v e) -> G k n -> G k n
-- onG f g = case g of
--   (G.Mesh (SG ms)) -> G.Mesh . SG . f $ ms 
--   (G.Transactor (SG ms)) -> G.Transactor . SG . f $ ms 
--   (G.Status (SG ms)) -> G.Status . SG . f $ ms
--   (G.Flow (SG ms)) -> G.Flow . SG . f $ ms

renderGrid :: forall m n. (MonadJSM m, CRUDChopaan m, ToJSON n) => SG n -> Html m ()
renderGrid sg = case sg of
  (G.Mesh (SG ms)) -> renderWith3 ms
  (G.Transactor (SG ms)) -> renderWith3 ms 
  (G.Status (SG ms)) -> renderWith3 ms
  (G.Flow (SG ms)) -> renderWith3 ms
