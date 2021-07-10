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
{-# LANGUAGE DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Chopaan.View where

import qualified Data.Text                         as T
import Data.Maybe (isNothing)
import Data.Proxy (Proxy(..))

import Control.Lens hiding (view, simple)
import Control.Lens.Unsound (lensProduct)
import Control.Monad.IO.Class



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
                                                    validate, Hygiene(..))
import           Shpadoinkle.Run             (runJSorWarp, simple, Env(Dev))
import           Shpadoinkle.Backend.ParDiff (runParDiff)


import Chopaan.Graph.Snapshot
import Servant.API                 ((:<|>) (..))
import Chopaan.Kibbutz.KbtzId
import Chopaan.API.History
import Chopaan.UiTypes
import Chopaan.Graph
import Chopaan.Graph.G as G (SG, G(..), SG'(..))
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
ainit _ = return MHomePage

defGView :: GView
defGView = GView (KbtzId "test") StatusG t0 t1 Nothing
  where
    t0 = Ti.UTCTime (Ti.fromGregorian 2021 4 6) (Ti.secondsToDiffTime 0)
    t1 = Ti.UTCTime (Ti.fromGregorian 2021 4 7) (Ti.secondsToDiffTime 0)

requestGView :: (CRUDChopaan m) => GView -> m (SG NodeMAC) 
requestGView (GView k g t0 t1 _) = getGraph k g t0 t1 


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
  RHomePage -> return $ MHomePage
  RKibbutzim -> MKibbutzim . RosterKbtzim (SortCol KId ASC) mempty <$> listKibbutzim
  RKibbutz k -> MKibbutz . RosterNodezim (SortCol NId ASC) mempty <$> (listNodezim k)
  RAddNode k -> return $ MAddNode k Nothing emptyNodeForm
  RGraph k -> loadG (defGView {_whichK = k})

homePage :: forall m a. MonadJSM m => Html m a
homePage = H.div
    [ H.class' $ Css.flex <> Css.flex_col <> Css.flex_grow <> Css.h_screen ]
    [ H.div headingBox [ H.h1 headingText [ "Welcome To Chopaan" ] ]
    , H.div menuBox
      [ H.div (headingBox  <> boxingCss) [
          H.a ([ H.onClickM_ . navigate @(SPA m) $ RKibbutzim ]) ["Add Kibbutz"]
          ]
      , H.div (headingBox <> boxingCss) [
          H.a ([ H.onClickM_ . navigate @(SPA m) $ RGraph (KbtzId "test") ]) ["View Kibbutzim"]
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
  MHomePage -> onSum _MHomePage $ homePage
  MKibbutzim kbtzRoster -> onSum _MKibbutzim $ H.div "container-fluid"
    [ H.div "row justify-content-between align-items-center"
     [ H.h2_ [ "Kibbutzim" ]
     , H.div [ H.class' "input-group"
             , H.textProperty "style" ("width:300px" :: T.Text)
             ]
       [ kbtzRoster <% searchK $ Input.search [ H.class' "form-control", H.placeholder "Search" ]
       , H.div "input-group-append mr-3"
         [ H.button [ H.onClickM_ $ navigate @(SPA m) RHomePage, H.class' "btn btn-primary" ] [ "Register" ]
         ]
       ]
     ]
   , onRecord (lensProduct tableK sortK) $ Table.viewWith tableCfg
       (kbtzRoster ^. tableK . to (KbtzList . (fuzzySearch fuzzyK $ kbtzRoster ^. searchK . value) . unKbtzList))
       (kbtzRoster ^. sortK)
    ]
  MKibbutz nodeRoster -> onSum _MKibbutz $ H.div "container-fluid"
    []
  MAddNode k n form -> onSum (_MAddNode . _3) $ H.div "row"
    [ H.div "col-sm-8 offset-sm-2"
      [ H.h1_ [ text $ maybe "Add New Node" (const "Edit Node") n
              ]
      , textControl @NodeMAC nodeMACU "MAC Address" (validate form) form
      , sectionTitle "Battery Details" "Edit Battery"
      , onRecord (hardwareConfigU . storageU) $ addBattery (form ^. hardwareConfigU ^. storageU)
      , sectionTitle "Solar Panel Details" "Edit Solar Panel"
      , onRecord (hardwareConfigU . generationU) $ addGeneration (form ^. hardwareConfigU . generationU)
      , sectionTitle "Load Details" "Edit Load"
      , onRecord (hardwareConfigU . loadU) $ addLoad (form ^. hardwareConfigU . loadU)
      , H.div "d-flex flex-row justify-content-end"
        [ cancelButton
        , saveButton n (getValid . validate $ form) undefined undefined
        ]
      ]
    ]
    where
      sectionTitle cr ed = H.h3_ [ text $ maybe cr (const ed) n ]
  MGraph gv -> onSum (_MGraph) $ gView gv
                
addBattery :: (MonadJSM m) => StorageUpdate 'Edit -> Html m (StorageUpdate 'Edit)
addBattery bc = H.div [ H.onClick (\a-> undefined) ]
      [ realControl @WattHours capacity "Battery Capacity" errs bc
      , realControl @Volts minVoltage "Minimum Battery Voltage" errs bc
      , realControl @Volts maxVoltage "Maximum Battery Voltage" errs bc
      , selectControl @'One @BatteryType batteryType "Battery Type" errs bc
      ]
  where
    errs = validate bc

addGeneration :: (MonadJSM m) => GenerationUpdate 'Edit -> Html m (GenerationUpdate 'Edit)
addGeneration ef = H.div genProps [
  realControl @Watts genPower "Panel Power" errs ef
  , realControl @Volts openCircuitVoltage "Open Circuit Voltage" errs ef
  , realControl @Volts voltageAtMPP "Voltage @ Max Power Point" errs ef
  , realControl @Amperes currentAtMPP "Current @ Max Power Point" errs ef
  ]
  where
    genProps = []
    errs = validate ef

addLoad :: (MonadJSM m) => LoadUpdate 'Edit -> Html m (LoadUpdate 'Edit) 
addLoad ef = H.div loadProps 
  [ realControl @Watts (loadPowerU) "Load Power" errs ef
  , realControl @Hours (loadDuration) "Load Duration" errs ef
  ]
  where
    loadProps = []
    errs = validate ef


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
    [ H.link'
        [ H.rel "stylesheet"
        , H.href "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.3.1/css/bootstrap.min.css"
        ]
    -- , H.link'
    --     [ H.rel "stylesheet"
    --     , H.href "https://unpkg.com/tailwindcss@2.1.2/dist/tailwind.min.css"
    --     ]
    , H.meta [ H.charset "ISO-8859-1" ] []
    , H.meta [ H.name' "viewport", H.content "width=device-width, initial-scale=1.0"] []
    --, H.script [ H.src $ entrypoint ev ] []
    ]
  , H.body_
    [ s
    ]
  ]


template :: Env -> Frontend -> Html m a -> Html m a
template ev fe stage = H.html_
  [ H.head_
    [ H.link'
        [ H.rel "stylesheet"
        , H.href "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.3.1/css/bootstrap.min.css"
        ]
    , H.link'
        [ H.rel "stylesheet"
        , H.href "./assets/tailwind.min.css"
        ]
    , H.meta [ H.charset "ISO-8859-1" ] []
    , H.meta [ H.name' "viewport", H.content "width=device-width, initial-scale=1.0"] []
    , toHydration fe
    , H.script [ H.src $ entrypoint ev ] []
    ]
  , H.body_
    [ stage
    ]
  ]

tableCfg :: Table.Theme m KbtzList
tableCfg = mempty
  { tableProps = const . const . pure $ H.class' "table table-striped table-bordered"
  , tdProps    = const . const . const $ \case
      _      -> "align-middle"
  }


  
fuzzyK :: [Kbtzim -> T.Text]
fuzzyK = flip (^.) <$>
  [ kbtzId   . to (T.pack . show)
  , kbtzName . to (T.pack . show)
  , kbtzDesc . to (T.pack . show)
  ]



gView :: forall m. (MonadJSM m, CRUDChopaan m) => GView -> Html m (GView)
gView g = H.div [H.class' $ Css.relative <> Css.flex_grow <> Css.flex_col]
  [ case _currentG g of
      Nothing -> voidC $ H.text "No Graph Found Yet"
      Just sg -> voidC $ renderKbtzGraph sg
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
      | gt <- [(minBound @GraphType)..maxBound] ]

