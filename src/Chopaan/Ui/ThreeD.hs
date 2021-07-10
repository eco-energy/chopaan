{-# LANGUAGE OverloadedStrings, TypeApplications, ScopedTypeVariables, OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules #-}

module Chopaan.Ui.ThreeD where

import GHC.Generics
import Control.DeepSeq
import Data.Aeson
import qualified Data.Text as T
import qualified Shpadoinkle.Html as H
import Shpadoinkle.Html.Utils (getBody)
import Shpadoinkle (Html)
import Shpadoinkle.Run (simple, runJSorWarp, live)
import Shpadoinkle.Widgets.Types (Humanize(..))
import Shpadoinkle.Backend.ParDiff
import Shpadoinkle.Lens
import Control.Lens (_1, _2)
import Data.Generics.Product
import Data.Generics.Labels

default(T.Text)

data Camera = Camera
  { fov :: Double
  , style :: T.Text }
  deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

data Scene a = Scene [a]
  deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

defCam = Camera 0 ""

data ThreeModel a = ThreeModel
  { scene :: Scene a
  , camera :: Camera
  }
  deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

defMod xs = ThreeModel (Scene xs) defCam

threeD :: forall m a. (Functor m, Humanize a) => ThreeModel a -> Html m (ThreeModel a)
threeD (ThreeModel (Scene xs) (Camera fov style)) = onRecord (#scene) $ H.div rootCSS $ [
  H.div cameraCSS $ (H.div objCSS . pure . H.text . humanize) <$> xs
  ]
  where
    rootCSS = [ H.textProperty "style" "overflow:hidden"]
    objCSS = [ H.textProperty "style" "position:absolute"]
    cameraCSS = [ H.textProperty "style" "transform-style:preserve-3d"]



main :: IO ()
main = do
  let model = defMod ["GodawfulPog" :: T.Text]
  live 8080 $ simple runParDiff model threeD getBody
