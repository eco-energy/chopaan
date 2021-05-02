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

import GHC.Generics (Generic)
import qualified Data.Text                         as T
import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (isNothing)

import Control.DeepSeq (NFData)
import Control.PseudoInverseCategory
import Control.Newtype.Generics

import Control.Lens hiding (view, simple)
import Control.Lens.Unsound (lensProduct)


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
import           Shpadoinkle.Widgets.Table         as Table hiding (view)
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
--import           Shpadoinkle.Console         (askJSM, trapper)
import           Shpadoinkle.Run             (runJSorWarp, simple, Env(Dev))
import           Shpadoinkle.Backend.ParDiff (runParDiff)



import NetSpider.Snapshot
import Chopaan.Kibbutz.KbtzId
import Chopaan.API.History
import Chopaan.UiTypes
import Chopaan.Graph
import Chopaan.CRUD
import Chopaan.Node.NodeT
import Chopaan.Node.NodeId
import Chopaan.Node.HW
import Chopaan.Node.Components
import Chopaan.Kibbutz.KbtzimT
import Chopaan.Ui.FormCommon
import Chopaan.Graph
import Chopaan.Ui.GraphView
import Data.Map
import qualified Clay as C
import Data.Colour
import qualified Algebra.Graph.Labelled as AG

import qualified Algebra.Graph as G


default (T.Text, [])

main :: IO ()
main = runJSorWarp 8080 $ do
  H.setTitle "Chopaan"
  simple runParDiff initial ((template Dev initial) . view) H.getBody
  where
    initial = (MAddNode (KbtzId "this") Nothing emptyNodeForm)

init :: (MonadJSM m) => Route -> m Frontend
init _ = return (MAddNode (KbtzId "this") Nothing emptyNodeForm)
  -- return MHomePage --

onRouteChange :: (Monad m, CRUDChopaan m) => Route -> m Frontend
onRouteChange = \case
  RHomePage -> return $ MHomePage
  RKibbutzim -> MKibbutzim . RosterKbtzim (SortCol KId ASC) mempty <$> listKibbutzim
  RKibbutz k -> MKibbutz . RosterNodezim (SortCol NId ASC) mempty <$> (listNodezim k)
  RAddNode k -> return $ MAddNode k Nothing emptyNodeForm
  -- RSearch k s -> MKibbutz . RosterNodezim (SortCol NId ASC) s <$> (listNodezim k) 
  

view :: forall m. (MonadJSM m) => Frontend -> Html m Frontend
view fe = case fe of
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
  MHomePage -> H.div_
    [ H.h1_ ["Welcome To Chopaan"]
    , H.a [ H.onClickM_ . navigate @(SPA m) $ RKibbutzim ] ["Add Kibbutz"]
    , H.a [ H.onClickM_ . navigate @(SPA m) $ RKibbutzim ] ["View Kibbutzim"]
    ] 
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
    isValid = getValid errs

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
    isValid = getValid errs

addLoad :: (MonadJSM m) => LoadUpdate 'Edit -> Html m (LoadUpdate 'Edit) 
addLoad ef = H.div loadProps 
  [ realControl @Watts (loadPowerU) "Load Power" errs ef
  , realControl @Hours (loadDuration) "Load Duration" errs ef
  ]
  where
    loadProps = []
    errs = validate ef
    isValid = getValid errs


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


graphView :: forall m n a e. (MonadJSM m, CRUDChopaan m, HistoryConn n a e) => SnapshotGraph n a e -> Html m (SnapshotGraph n a e)
graphView (nodes, links) = H.div "container-graph"
  [ H.canvas [] [] 
  ]



template :: Env -> Frontend -> Html m a -> Html m a
template ev fe stage = H.html_
  [ H.head_
    [ H.link'
      [ H.rel "stylesheet"
      , H.href "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.3.1/css/bootstrap.min.css"
      ]
    , H.meta [ H.charset "ISO-8859-1" ] []
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



--adaptK :: KbtzList -> [Kbtzim -> T.Text]
--adaptK = fuzzyK
  
fuzzyK :: [Kbtzim -> T.Text]
fuzzyK = flip (^.) <$>
  [ kbtzId   . to (T.pack . show)
  , kbtzName . to (T.pack . show)
  , kbtzDesc . to (T.pack . show)
  ]



gView :: (MonadJSM m) => GView -> Html m GView
gView gv = H.div_
  [ onRecord sg $ renderKbtzGraph (gv ^. sg)
  , onRecord whichG $ graphSelectButtons ]
  where
    graphSelectButtons :: Html m (GraphType)
    graphSelectButtons = H.div_ [
      H.button [ H.onClick $ (const g)
               , H.class' "btn btn-primary" ] [ text . humanize $ g ]
      | g <- [(minBound @GraphType)..maxBound] ]
