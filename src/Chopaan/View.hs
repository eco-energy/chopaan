{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Chopaan.View where

import qualified Data.Text                         as T
import Data.Aeson (ToJSON)

import Control.PseudoInverseCategory
import           Shpadoinkle                       (Html, MonadJSM, text)
import qualified Shpadoinkle.Html                  as H
import           Shpadoinkle.Lens
import           Shpadoinkle.Router                (navigate, toHydration)
import           Shpadoinkle.Run                   (Env, entrypoint)
import           Shpadoinkle.Widgets.Form.Dropdown as Dropdown (Dropdown (..),
                                                                Theme (..),
                                                                defConfig,
                                                                dropdown)
import qualified Shpadoinkle.Widgets.Form.Input    as Input
import           Shpadoinkle.Widgets.Table         as Table
import           Shpadoinkle.Widgets.Types         (Consideration, Considered,
                                                    ConsideredChoice,
                                                    Control (..), Field,
                                                    Hygiene (..), Input (..),
                                                    Pick (..), Present,
                                                    Selected, Status (..),
                                                    Toggle (..), Validated (..),
                                                    fullset, fuzzySearch,
                                                    getValid, humanize, present,
                                                    validate, withOptions')

import Chopaan.UiTypes
import Chopaan.Graph
import Chopaan.CRUD
import Chopaan.Node.NodeT

default (T.Text, [])



start :: (Monad m, CRUDChopaan m) => Route -> m Frontend
start = \case
  REcho t -> return $ MEcho t 
  RKibbutzim -> MKibbutzim . RosterKbtzim (SortCol KId ASC) mempty <$> listKibbutzim
  RKibbutz k -> MKibbutz . RosterNodezim (SortCol NId ASC) mempty <$> (listNodezim k)
  RAddNode k -> return $ MAddNode k Nothing emptyNodeForm
  -- RSearch k s -> MKibbutz . RosterNodezim (SortCol NId ASC) s <$> (listNodezim k) 
  

view :: forall m. (MonadJSM m, CRUDChopaan m) => Frontend -> Html m Frontend
view fe = case fe of
  MKibbutzim kbtzRoster -> onSum _MKibbutzim $ H.div "container-fluid"
    []
  MKibbutz nodeRoster -> onSum _MKibbutz $ H.div "container-fluid"
    []
  MEcho t -> H.div_
    [maybe (text "Eerie Silence") text t
    , H.a [ H.onClickM_ . navigate @(SPA m) $ RKibbutzim ] ["View Kibbutzim"]
    ]
  


graphView :: forall m n a e. (MonadJSM m, CRUDChopaan m, HistoryConn n a e) => SnapshotGraph n a e -> Html m (SnapshotGraph n a e)
graphView g = H.div "container-graph"
  [ H.canvas [] []
  ]


renderGraph :: forall m n e a. (MonadJSM m, HistoryConn n e a)
  => (a -> Html m (Graph e a))
  -> (e -> Html m (Graph e a) -> Html m (Graph e a) -> Html m (Graph e a))
  -> Graph e a
  -> Html m (Graph e a)
renderGraph = foldg (H.div' [ H.onClick id ]) 


tradGraph :: forall n m e a. (MonadJSM m, HistoryConn n e a, ToJSON e, ToJSON a
                           , Renderable a, Renderable e, Show a, Show e)
  => Graph e a
  -> Html m (Graph e a)
tradGraph = renderGraph @m @n (n . render @a) edgeH

class Renderable a where
  render :: forall m. a -> Html m a

vertexGIso :: (Show a, Show e) => EndoIso a (Graph e a) 
vertexGIso = EndoIso id vertex (\g -> case g of
                                   (Vertex a) -> a
                                   x ->
                                     error
                                      ("Cannot deal with any node other than a vertex: Recieved\n"
                                       <> show x))

edgeGIso :: (Ord a, Eq e, Monoid e) => EndoIso (e, a, a) (Graph e a)
edgeGIso = EndoIso id (\c -> edges [c]) (head . edgeList)



n :: (Applicative m, Show a, Show e) => Html m a -> Html m (Graph e a)
n = pimap vertexGIso

e :: (Applicative m, Ord a, Eq e, Monoid e) => Html m (e, a, a) -> Html m (Graph e a)
e = pimap edgeGIso


sphereH :: Renderable a => a -> Html m a
sphereH a = H.canvas [H.onClick id] [ render a ]


edgeH :: (Renderable e) => e -> Html m (Graph e a) -> Html m (Graph e a) -> Html m (Graph e a)
edgeH = undefined
