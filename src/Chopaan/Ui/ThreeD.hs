{-# LANGUAGE OverloadedStrings, TypeApplications, ScopedTypeVariables, OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules, TypeFamilies #-}

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
import Data.Key as K

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


cameraCSSMatrix :: Camera -> (T.Text, H.Prop m Camera)
cameraCSSMatrix c = H.textProperty "style" ("transform: " <> mkCameraStyle c)
  where
    mkCameraStyle c = ""

objectCSSMatrix :: forall f a. (Key f ~ Int, Functor f, Keyed f, Foldable f, Fractional a, Num a, Ord a, Show a) => f a -> T.Text
objectCSSMatrix mat = foldl (\x y -> x <> ((uncurry epsilon y) <> ",")) "matrix3d(" $
                      K.keyed mat
  where
    epsilon ::  Int -> a -> T.Text
    epsilon i x = if (abs x) < 1e-10
                  then (T.pack . show $ (0 :: a))
                  else (T.pack . show . sgn i $ x)
    sgn i z = if (((mod i 4) - 2) == 0) then (z * (-1)) else z 
    
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
