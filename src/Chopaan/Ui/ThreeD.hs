{-# LANGUAGE OverloadedStrings, TypeApplications, ScopedTypeVariables, OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules, TypeFamilies #-}

module Chopaan.Ui.ThreeD where

import GHC.Generics
import Control.DeepSeq
import Control.Monad
import Data.Aeson
import Data.Functor.Rep

import qualified Data.Text as T
import qualified Shpadoinkle.Html as H
import Shpadoinkle.Html.Utils (getBody)
import Shpadoinkle (Html)
import Shpadoinkle.Run (simple, runJSorWarp, live)
import Shpadoinkle.Widgets.Types (Humanize(..))
import Shpadoinkle.Backend.ParDiff
import Shpadoinkle.Lens
import Control.Lens hiding (simple)
import Data.Generics.Product
import Data.Generics.Labels
import qualified Data.Key as K

import Linear.Vector
import Linear.Matrix
import Linear.V3
import Linear.Quaternion


-- $ A Translation of
-- $ https://github.com/mrdoob/three.js/blob/dev/examples/jsm/renderers/CSS3DRenderer.js
-- $ https://github.com/mrdoob/three.js/blob/dev/src/core/Object3D.js
-- $ https://github.com/mrdoob/three.js/blob/dev/examples/jsm/controls/TrackballControls.js
-- $ https://github.com/mrdoob/three.js/blob/dev/examples/css3d_periodictable.html


default(T.Text)

type V3R = V3 Double 

type QuatR = Quaternion Double

type M44R = M44 Double



data Obj = Obj
  { pos :: V3R
  , rot :: QuatR
  , scale :: V3R
  } deriving (Eq, Ord, Show, Generic, NFData)

defObj = Obj zero zero zero

data Camera = Camera
  { fov :: Double
  , style :: T.Text }
  deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

data Scene a = Scene [(a, Obj)]
  deriving (Eq, Show, Generic, NFData)

mkScene :: [a] -> Scene a
mkScene = Scene . (flip zip (repeat defObj))

defCam = Camera 0 ""

data ThreeModel a = ThreeModel
  { scene :: Scene a
  , camera :: Camera
  }
  deriving (Eq, Show, Generic, NFData)

epsilon :: (Functor f, RealFrac a, Ord a) => f a -> f a
epsilon = fmap ep
  where
    ep x = if (abs x) < 1e-10 then 0 else  x

cssMat :: M44R -> T.Text
cssMat m = (foldl (\x y -> x <> "," <> ((T.pack . show $ y))) "matrix3d("  $ join m) <> ")"

toCSSMatEp :: M44R -> T.Text
toCSSMatEp = cssMat . (fmap epsilon)


cameraCSSMatrix :: Camera -> (T.Text, H.Prop m Camera)
cameraCSSMatrix c = H.textProperty "style" ("transform: " <> mkCameraStyle c)
  where
    mkCameraStyle c = ""

objectCSSMatrix :: M44R -> T.Text
objectCSSMatrix = cssMat . negativeY
  where
    -- Second Row should be negative
    negativeY = over _y negated
    
defMod xs = ThreeModel (mkScene xs) defCam

threeD :: forall m a. (Functor m, Humanize a) => ThreeModel a -> Html m (ThreeModel a)
threeD (ThreeModel (Scene xs) (Camera fov style)) = onRecord (#scene) $ H.div rootCSS $ [
  H.div cameraCSS $ (H.div objCSS . pure . H.text . humanize . fst) <$> xs
  ]
  where
    rootCSS = [ H.textProperty "style" "overflow:hidden"]
    objCSS = [ H.textProperty "style" "position:absolute"]
    cameraCSS =
      [ H.textProperty "style" "transform-style:preserve-3d"
      , H.textProperty "style" "pointer-events: none"
      ]



main :: IO ()
main = do
  let model = defMod ["GodawfulPog" :: T.Text]
  live 8080 $ simple runParDiff model threeD getBody
