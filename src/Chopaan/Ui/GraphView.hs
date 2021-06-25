{-# LANGUAGE FlexibleInstances, TypeFamilies, InstanceSigs
, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints, TemplateHaskell
, RankNTypes, FlexibleContexts, AllowAmbiguousTypes, ScopedTypeVariables, GADTs, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings, PackageImports, ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase, TypeOperators, TypeApplications, LiberalTypeSynonyms  #-}
module Chopaan.Ui.GraphView where

import ConCat.Misc (inNew, inNew2, (:*), (:+), Unop)
import GHC.Generics (Generic, Generic1)
import qualified Control.Newtype.Generics as N
import Control.DeepSeq (NFData)
import Control.PseudoInverseCategory
import Data.Aeson as A
import Data.Text hiding (empty, zip)
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding as T
import qualified Data.Map as M
import Data.Map (Map)
import Data.Bifunctor
import Data.Typeable
import Data.Int
import Lens.Micro

import Text.InterpolatedString.Perl6 (q)

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
import           Language.Javascript.JSaddle
import           Shpadoinkle
import           UnliftIO.Concurrent                     (forkIO, threadDelay)


import Shpadoinkle (Html(..), liftC, text, MonadJSM, Continuation, Html,
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

import qualified Algebra.Graph.Labelled as AG
import qualified Algebra.Graph as G
--import Algebra.Graph.Label

--import Diagrams.Prelude
import qualified Diagrams.Envelope as E
import Diagrams.Backend.SVG (B)
import qualified Diagrams.TwoD.Text as DT
import Diagrams.TwoD.Layout.Grid
import Graphics.SVGFonts
import qualified Clay as C

import NetSpider.Snapshot
import qualified NetSpider.Timestamp as N

import Shpadoinkle.Template.TH

import Chopaan.Node.Mesh
import Chopaan.Node.Folds
import Chopaan.Kibbutz.Transactor
import Chopaan.Graph
import Chopaan.Graph.G as G
import Chopaan.Node.NodeId
import qualified Chopaan.Ui.Style as Css 
import Chopaan.CRUD
import Data.FileEmbed

default (Text)

-- newtype Pos = Pos Double
--   deriving stock (Generic)
--   deriving newtype (Fractional, Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
--   deriving anyclass (Humanize, Present, NFData)
--   deriving (Semigroup, Monoid) via (Sum Double)



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
    renderThis :: forall v l.
      (Monoid l, Eq l, Ord v, Show l, Show v)
      =>  SnapshotGraph n v l -> Html m ()--(SnapshotGraph n v l)
    renderThis (ns, ls) = H.div
      [ H.class' $ Css.flex <> Css.flex_grow]
      (
        [ H.div (nodeClasses i) $ [ nodeHtml n ] | (i, n) <- zip [0,(1 :: Double)..] $ ns]
        <>
        [ H.div (edgeClasses i) $ [ edgeHtml l n n' ]
        | (i, (l, (n, _), (n', _))) <- zip [(0 :: Double), 1..] $ castLinks (ns, ls)
        ] <> [ cview ]
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
renderWith3 a = baked $ do
  -- $(embedHtml "src/Chopaan/Ui/gridRenderer.html")
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
  ] $(embedHtml "src/Chopaan/Ui/gridRenderer.html")

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
