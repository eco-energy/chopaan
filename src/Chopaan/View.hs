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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Chopaan.View where

import Control.Lens hiding (view, simple)
import Control.Lens.Unsound (lensProduct)
import Control.Monad.IO.Class

import qualified Data.Text                         as T
import Data.Maybe (isNothing)
import Data.Proxy (Proxy(..))
import Data.Generics.Product
import Data.Generics.Sum
import Data.Generics.Labels
import Data.Maybe


import           Shpadoinkle                       (Html, MonadJSM, text, voidC)
import qualified Shpadoinkle.Html                  as H
import qualified Shpadoinkle.Html.Utils            as H
import           Shpadoinkle.Lens
import           Shpadoinkle.Router                (navigate, toHydration)
import           Shpadoinkle.Router.Client   (client, runXHR)
import           Shpadoinkle.Run                   (Env, entrypoint)

import qualified Shpadoinkle.Widgets.Form.Input    as Input
import qualified Shpadoinkle.Widgets.Types.Form    as F 

import           Shpadoinkle.Widgets.Table         as Table hiding (view)
import           Shpadoinkle.Widgets.Types         (Control (..),
                                                    Pick (..), Status (..),
                                                    fuzzySearch,
                                                    getValid, humanize,
                                                    validate, Hygiene(..), Search)
import           Shpadoinkle.Run             (runJSorWarp, simple, Env(Dev))

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL

import Chopaan.Graph.Snapshot
import Servant.API                 ((:<|>) (..))
import Chopaan.Kibbutz.KbtzId
import Chopaan.API.History
import Chopaan.UiTypes
import Chopaan.Graph
import Chopaan.Graph
import Chopaan.CRUD
import Chopaan.Node.NodeT
import Chopaan.Node.NodeId
import Chopaan.Node.HW
import Chopaan.Node.Components
import Chopaan.Kibbutz.KbtzimT
import Chopaan.Ui.FormCommon
import Chopaan.Ui.GraphView
import qualified Chopaan.Ui.Style as Css
import qualified Data.Time as Ti

default (T.Text, [])


ainit :: (Monad m, CRUDChopaan m) => Route -> m Frontend
ainit _ = MHomePage . RosterKbtzim (SortCol KId ASC) mempty <$> listKibbutzim

defGView :: GView
defGView = GView (KbtzId "test") StatusG t0 t1 Nothing
  where
    t0 = Ti.UTCTime (Ti.fromGregorian 2021 4 6) (Ti.secondsToDiffTime 0)
    t1 = Ti.UTCTime (Ti.fromGregorian 2021 4 7) (Ti.secondsToDiffTime 0)

requestGView :: forall m. (CRUDChopaan m, Monad m) => GView -> m (SG NodeMAC) 
requestGView (GView k g t0 t1 _) = case g of
  MeshG -> getL Mesh getMesh
  PlanG -> getL Transactor getTransactor
  StatusG -> getL Status getStatus
  FlowG -> getL Flow getFlow
  where
    getL :: forall a b. (SG' NodeMAC a b -> SG NodeMAC)
         -> (SG NodeMAC -> Maybe (SG' NodeMAC a b))
         -> m (SG NodeMAC)
    getL c p = c <$> (S.fold FL.mconcat
                             $ S.map (fromJust)
                                   $ S.filter (isJust)
                                   $ S.map p
                                   $ getGraph k g t0 t1)

mkGView :: KbtzName -> GraphType -> Ti.UTCTime -> Ti.UTCTime -> GView 
mkGView k g t0 t1 = GView k g t0 t1 Nothing

ginitM :: (MonadIO m, CRUDChopaan m) => Route -> m Frontend
ginitM _ = do
  g <- (requestGView defGView)
  liftIO . print $ g
  return $ MGraph (defGView { _currentG = (Just g) })


loadGraph :: (Monad m, CRUDChopaan m) => GView -> m (GView)
loadGraph gv = do
  g <- (requestGView gv)
  return $ (gv { _currentG = (Just g) })

loadG g = (pure . MGraph) =<< loadGraph g



onRouteChange :: (Monad m, CRUDChopaan m) => Route -> m Frontend
onRouteChange = \case
  RHomePage -> MHomePage . RosterKbtzim (SortCol KId ASC) mempty <$> listKibbutzim
  RKibbutzim -> MKibbutzim . RosterKbtzim (SortCol KId ASC) mempty <$> listKibbutzim
  RKibbutz k -> MKibbutz . RosterNodezim (SortCol NId ASC) mempty <$> (listNodezim k)
  RAddNode k -> return $ MAddNode k Nothing emptyNodeForm
  RGraph k -> loadG (defGView {_whichK = k})

homePage :: forall m a. MonadJSM m => RosterKbtzim -> Html m RosterKbtzim
homePage k = H.div
    [ H.class' $ Css.flex <> Css.flex_col <> Css.flex_grow <> Css.h_screen ]
    [ H.div headingBox [ H.h1 headingText [ "Welcome To Chopaan" ] ]
    , H.div menuBox
      [ H.div (headingBox  <> boxingCss) [
          H.a ([ H.onClickM_ . navigate @(SPA m) $ RKibbutzim ]) ["Add Kibbutz"]
          ]
      , H.div (headingBox <> boxingCss) [
          H.a ([ H.onClickM_ . navigate @(SPA m) $ RGraph (KbtzId "test") ]) ["View Kibbutzim"]
          ]
      , H.div (headingBox <> boxingCss) [
          H.div [ H.class' "input-group"
                , H.textProperty "style" ("width:300px" :: T.Text)
                ] [ k <% #_searchK $ Input.search [ H.class' "form-control", H.placeholder "Search" ] ]
          , onRecord (lensProduct #_tableK #_sortK) $ Table.viewWith tableCfg
            (k ^. #_tableK
             . to (KbtzList .
               (fuzzySearch searchKbtzName (k ^. (#_searchK . (value @(F.Input)))))
                    . unKbtzList))
            (_sortK k)
          ]
      ]
    ]
    where
      menuBox = [H.class'
                  $ Css.flex
                  <> Css.flex_row
                  <> Css.flex_auto
                  <> Css.grid
                  <> Css.grid_cols_2
                  <> Css.place_items_stretch
                  <> Css.h_full
                  <> Css.bg_black
                  <> Css.text_white
                ]
      headingBox = [ H.class' $ Css.h_full <> Css.grid <> Css.place_items_center ]
      headingText = [ H.class' $ Css.text_3xl <> Css.flex_grow]
      boxingCss = [ H.class'
                    $ Css.border_solid
                    <> Css.border_4
                    <> Css.border_blue_500
                    <> Css.h_full
                    <> Css.flex_grow
                    <> Css.text_center
                    <> "hover:underline"
                  ]


view :: forall m. (MonadJSM m, CRUDChopaan m) => Frontend -> Html m Frontend
view fe = case fe of
  MHomePage ks -> onSum #_MHomePage $ homePage ks
  MKibbutzim kbtzRoster -> onSum #_MKibbutzim $ H.div "container-fluid"
    [ H.div "row justify-content-between align-items-center"
     [ H.h2_ [ "Kibbutzim" ]
     , H.div [ H.class' "input-group"
             , H.textProperty "style" ("width:300px" :: T.Text)
             ]
       [ kbtzRoster <% #_searchK $ Input.search [ H.class' "form-control", H.placeholder "Search" ]
       -- , H.div "input-group-append mr-3"
       --   [ H.button [ H.onClickM_ $ navigate @(SPA m) RHomePage, H.class' "btn btn-primary" ] [ "Register" ]
       --   ]
       ]
     ]
   , onRecord (lensProduct #_tableK #_sortK) $ Table.viewWith tableCfg
       (kbtzRoster ^. #_tableK
         . to (KbtzList . (fuzzySearch searchKbtzName (kbtzRoster ^. #_searchK . (value @(F.Input)))) . unKbtzList))
       (_sortK kbtzRoster)
    ]
  MKibbutz nodeRoster -> onSum #_MKibbutz $ H.div "container-fluid"
    []
  MAddNode k n form -> onSum (#_MAddNode . _3) $ H.div "row"
    [ H.div "col-sm-8 offset-sm-2"
      [ H.h1_ [ text $ maybe "Add New Node" (const "Edit Node") n
              ]
      , textControl @NodeMAC #nodeMACU "MAC Address" (validate form) form
      , sectionTitle "Battery Details" "Edit Battery"
      , onRecord (#hardwareConfigU . #storageU) $ addBattery (form ^. #hardwareConfigU ^. #storageU)
      , sectionTitle "Solar Panel Details" "Edit Solar Panel"
      , onRecord (#hardwareConfigU . #generationU) $ addGeneration (form ^. #hardwareConfigU . #generationU)
      , sectionTitle "Load Details" "Edit Load"
      , onRecord (#hardwareConfigU . #loadU) $ addLoad (form ^. #hardwareConfigU . #loadU)
      , H.div "d-flex flex-row justify-content-end"
        [ cancelButton
        , saveButton n (getValid . validate $ form) undefined undefined
        ]
      ]
    ]
    where
      sectionTitle cr ed = H.h3_ [ text $ maybe cr (const ed) n ]
  MGraph gv -> onSum (#_MGraph) $ gView gv


cancelButton :: forall m a. MonadJSM m => Html m (NodeUpdate 'Edit)
cancelButton = H.button
      [ H.onClickM_ . navigate @(SPA m) $ RKibbutzim
      , H.class' "btn btn-secondary"
      ] [ "Cancel" ]


saveButton :: forall m a b. (MonadJSM m) =>
  (Maybe a) -> Maybe b -> (b -> m ()) -> (a -> b -> m ()) -> Html m (NodeUpdate 'Edit)
saveButton idT isValid createT updateT  = H.button
      [ H.onClickM_ $ case isValid of
         Nothing -> return ()
         Just up -> do
           case idT of Nothing  -> () <$ createT up
                       Just sid -> updateT sid up
           navigate @(SPA m) (RKibbutzim)
      , H.class' "btn btn-primary"
      , H.disabled $ isNothing isValid
      ] [ "Save" ]


staticTemplate :: (Monad m) => Html m () -> Html m b
staticTemplate s = voidC $ H.html_
  [ H.head_
    [ H.meta [ H.charset "ISO-8859-1" ] []
    , H.meta [ H.name' "viewport", H.content "width=device-width, initial-scale=1.0"] []
    , stylesheetAsset "tailwind.min.css"
    , stylesheet bootstrap
    --, H.script [ H.src $ entrypoint ev ] []
    ]
  , H.body_
    [ s
    ]
  ]

stylesheetAsset :: T.Text -> Html m a
stylesheetAsset = stylesheet . ("./assets/" <>)

stylesheet :: T.Text -> Html m a
stylesheet f = H.link'
        [ H.rel "stylesheet"
        , H.href f
        ]

bootstrap = "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.3.1/css/bootstrap.min.css"

template :: Env -> Frontend -> Html m a -> Html m a
template ev fe stage = H.html_
  [ H.head_
    [ H.meta [ H.charset "ISO-8859-1" ] []
    , H.meta [ H.name' "viewport", H.content "width=device-width, initial-scale=1.0"] []
    , stylesheetAsset "tailwind.min.css"
    , stylesheetAsset "style.css"
    , toHydration fe
    , H.script [ H.src $ entrypoint ev ] []
    ]
  , H.body_
    [ stage
    ]
  ]

tableCfg :: forall m . (MonadJSM m) => Table.Theme m KbtzList
tableCfg = mempty
  { tableProps = const . const . pure $ H.class' "table table-striped table-bordered"
  , tdProps    = const . const $ \(KbtzimRow k) -> \case
      _      -> [H.class' "align-middle", H.class' "text-white"] <> [H.onClickM_ . navigate @(SPA m) . RGraph $ k]
  }


  
searchKbtzName :: [KbtzName -> T.Text]
searchKbtzName = [ unKbtzId ]



gView :: forall m. (MonadJSM m, CRUDChopaan m) => GView -> Html m (GView)
gView g = H.div [H.class' $ Css.relative <> Css.flex_grow <> Css.flex_col]
  [ case _currentG g of
      Nothing -> voidC $ H.text "No Graph Found Yet"
      Just sg -> render3dGrid sg
  , graphSelectButtons
  --, onRecord whichK $ getGraph
  ]
  where
    graphSelectButtons :: Html m (GView)
    graphSelectButtons = H.div [H.class' $ Css.flex
                                 <> Css.flex_row
                                 <> Css.justify_center
                                 <> Css.w_full
                                 <> Css.content_end ]
      [ H.button [ H.onClickM (do
                                  gv' <- loadGraph (g {_whichG = gt})
                                  return (\g' -> g' {_currentG = _currentG gv'}))
               , H.class' $ Css.flex
                 <> Css.justify_center
                 <> Css.w_full
               ] [ text . humanize $ gt ]
      | gt <- [(minBound @GraphType)..maxBound]]

