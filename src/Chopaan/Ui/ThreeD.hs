{-# LANGUAGE OverloadedStrings, TypeApplications, ScopedTypeVariables, OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules, TypeFamilies, NamedFieldPuns, TemplateHaskell #-}

module Chopaan.Ui.ThreeD where

import GHC.Generics hiding (R)
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class

import Control.Concurrent
import Control.Concurrent.STM
import Data.Functor.Rep
import System.IO (stderr, hPutStrLn, stdout, hFlush)

import qualified Data.Text as T
import           Data.FileEmbed              (embedFile)
import           Data.Text.Encoding          (decodeUtf8)


import GHCJS.DOM (currentWindow, currentWindowUnchecked)
import GHCJS.DOM.Window (getInnerHeight, getInnerWidth, Window, requestAnimationFrame)
import GHCJS.DOM.RequestAnimationFrameCallback (newRequestAnimationFrameCallback)

import Shpadoinkle (Html, JSM, MonadJSM, TVar, voidC, shpadoinkle)
import Shpadoinkle.Run (simple, runJSorWarp, live)
import qualified Shpadoinkle.Html as H
import Shpadoinkle.Html.Utils (getBody)
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
import Linear.V4
import Linear.Quaternion
import Linear.Metric
import Linear.Projection

import qualified Chopaan.Ui.Style as Css

-- $ A Translation of
-- $ https://github.com/mrdoob/three.js/blob/dev/examples/jsm/renderers/CSS3DRenderer.js
-- $ https://github.com/mrdoob/three.js/blob/dev/src/core/Object3D.js
-- $ https://github.com/mrdoob/three.js/blob/dev/examples/jsm/controls/TrackballControls.js
-- $ https://github.com/mrdoob/three.js/blob/dev/examples/css3d_periodictable.html


default(T.Text)

type R = Double

type V3R = V3 R 

type QuatR = Quaternion R

type M44R = M44 R


data Obj = Obj
  { _pos :: V3R
  , _rot :: QuatR
  , _scale :: V3R
  , _localTransform :: M44R
  , _worldTransform :: M44R
  } deriving (Eq, Ord, Show, Generic, NFData)

translateObj :: V3R -> Obj -> Obj
translateObj t o = o
                   & #_pos .~ newP
                   & (#_localTransform . translation) .~ newP
                   & (#_worldTransform . translation) .~ newP
  where
    newP = t ^+^ (_pos o)

rotateObj :: QuatR -> Obj -> Obj
rotateObj q o = o
                & #_rot .~ newQ
                & (#_localTransform . _m33) %~ newQT
                & (#_worldTransform . _m33) %~ newQT
  where
    newQ = q * (_rot o)
    newQT p = (fromQuaternion q !*! p)

scaleObj :: V3R -> Obj -> Obj
scaleObj s o = o
               & #_scale .~ s
               & (#_localTransform . _m33) %~ newS
               & (#_worldTransform . _m33) %~ newS
  where
    newS p = (scaled s !*! p)


transformMat :: V3R -> QuatR -> V3R -> M44R
transformMat p r s = mkTransformationMat ((scaled s) !*! (fromQuaternion r)) p

asT :: Obj -> M44R
asT Obj{_pos, _rot, _scale} = transformMat _pos _rot _scale

mkObj :: V3R -> QuatR -> V3R -> Obj
mkObj pos rot scale = Obj pos rot scale locT locT
  where
    locT = transformMat pos rot scale

defQuat = axisAngle (V3 1 1 1) 

zeroObj :: Obj
zeroObj = mkObj zero zero (V3 1 1 1)


data Camera = Camera
  { fov :: Double
  , aspect :: Double
  , near :: Double
  , far :: Double
  , matrixWorldInverse :: M44R
  , projectionTransform :: M44R
  , cameraObj :: Obj
  }
  deriving (Eq, Show, Generic, NFData)


toRad = (* (pi / 180))
nanEr v x = if (isNaN x) then error (v <> " is NaN") else x
matNaN v = (fmap (fmap (nanEr v)))
hasNaN :: (Functor f, Functor g, Foldable f, Foldable g, RealFloat a) => f (g a) -> Bool
hasNaN = isNaN . sum . (fmap sum)

mkCam :: Obj -> Double -> Double -> Double -> Double -> Camera
mkCam obj fov asp near far = Camera fov asp near far worldInv perspectiveProj obj
  where
    perspectiveProj = perspective fov asp near far
    perspectiveProj' :: M44R
    perspectiveProj' = zero & (column _x) %~ (_x .~ x)
                           & (column _y) %~ (_y .~ y)
                           & (column _z) .~ (V4 a b c (-1))
                           & (column _w) %~ (_z .~ d)
      where
        zoom = 1.0
        top =  near * (tan $ (toRad fov) * 0.5 * fov) / zoom
        height =  2 * top
        width =  asp * height
        left =  0.5 * width
        right =  left + width
        bottom =  top - height
        x =  2 * near / (right - left)
        y =  2 * near / (top - bottom)
        a =  (right + left) / width
        b =  (top + bottom) / height
        c =  (- (far + near) / (far - near))
        d =  (- 2) * (far * near) / (far - near)
    worldInv = inv44 $ asT obj

worldDirection :: Camera -> V3 R
worldDirection = normalize . (view (_xyz . column _z)) . matrixWorldInverse 

data Scene a = Scene [(a, Obj)]
  deriving (Eq, Show, Generic, NFData)

mkScene :: (Int -> Obj) -> [a] -> Scene a
mkScene objF = Scene . (flip zip (objF <$> [0,1..]))

data ThreeModel a = ThreeModel
  { scene :: Scene a
  , camera :: Camera
  , widthG :: R
  , heightG :: R
  } deriving (Eq, Show, Generic, NFData)

epsilon :: (Functor f, RealFrac a, Ord a) => f a -> f a
epsilon = fmap ep
  where
    ep x = if (abs x) < 1e-10 then 0 else  x

cssMat :: M44R -> T.Text
cssMat m = "matrix3d(" <> (foldMat ""  (transpose m)) <> ")"
  where
    foldMat :: T.Text -> M44R -> T.Text
    foldMat x y = T.init $ foldl foldVec x y
    foldVec :: T.Text -> (V4 Double) -> T.Text
    foldVec i v = foldl (\x y -> x <> ((textS $ y) <> ",")) i v

cssMatEp :: M44R -> T.Text
cssMatEp = cssMat . (fmap epsilon)


cameraCSSMat :: Camera -> T.Text
cameraCSSMat c = (tz (fov c) <>) . cssMatEp . negativeRowY . matrixWorldInverse $ c 
  where
    -- Second Row Should be Negative
    tz x = "translateZ(" <> (textS $ x) <> "px)"
    negativeRowY = over _y negated
    
objectCSSMat :: Obj -> T.Text
objectCSSMat = ("translate(-50%, -50%)" <>) . cssMatEp . negativeColY . _worldTransform
  where
    -- Second Column should be negative
    negativeColY = over (column _y) negated


defCam :: Double -> Double -> Double -> Camera
defCam z w h = mkCam objZ 40 (w / h) 1 10000
  where
    objZ :: Obj
    objZ = translateObj (zero & _z .~ z) zeroObj

defMod :: MonadJSM m => (Int -> Obj) -> [a] -> m (ThreeModel a)
defMod objF xs = do
  w' <- currentWindow
  case w' of
    Nothing -> error "NO WINDOW"
    Just w -> do
      (width, height) <- getWH
      let cam = (defCam 3000 width height)
      return $ ThreeModel (mkScene (objF' (_rot . cameraObj $ cam)) xs) cam width height
  where
    objF' q = (rotateObj q) . objF


getWH = do
  w <- currentWindowUnchecked
  height <- realToFrac <$> getInnerHeight w
  width <- realToFrac <$> getInnerWidth w
  return (width, height)

-- 
-- 

threeD :: forall m a. (MonadJSM m, Humanize a) => ThreeModel a -> Html m (ThreeModel a)
threeD (ThreeModel (Scene xs) c widthG heightG) = H.div rootProps [
  H.div (cameraCSS) $
    (\(x, y) -> H.div (objCSS y) . pure . H.text . humanize $ x) <$> xs
  ]
  where
    rootProps = rootHandler <> rootCSS
    resizeHandler = do
      (w, h) <- getWH
      return $ \(ThreeModel s c _ _) -> (ThreeModel s c w h) 
    rootHandler = [H.onResizeM resizeHandler]
    fov' = (c ^. #projectionTransform . _y . _y) * (heightG / 2)
    rootCSS = [ H.textProperty "id" "renderer"
              , styleP "overflow:hidden"
              , styleP ("perspective:" <> (textS fov') <> "px")
              , H.class' Css.w_screen
              , H.class' Css.h_screen
              , styleP "background-color: black"
              ]
    cameraCSS =
      [ H.textProperty "id" "camera"
      , transformP $ (cameraCSSMat c) <> (translatePx (widthG / 2) (heightG / 2))
      , styleP "transform-style: preserve-3d"
      , styleP "pointer-events: none"
      , H.class' Css.w_screen
      , H.class' Css.h_screen
      ]
      
    objCSS y = [ styleP "position:absolute"
               , styleP "pointer-events: auto"
               , transformP $ objectCSSMat y
               , H.class' "element"
               , styleP "background-color: white"
               ]


grid3D :: Int -> Int -> Int -> (Int -> Obj)
grid3D row col stack = (gridPos)
  where
    gridPos :: Int -> Obj
    gridPos i = translateObj (V3 (x i) (y i) (z i)) zeroObj
    x i = c $ (mod i row) * 400 + 800
    y i = c $ (- (mod (div i col) col)) * 400 + 800
    z i = c $ ((div i stack)) * 1000 - 2000
    c = fromIntegral @Int @Double


styleP = H.textProperty "style"
transformP x = styleP $ "transform:" <> x
translatePx w h = "translate(" <> (toPx w) <> "," <> (toPx h)
toPx = (<> "px") . textS
textS = T.pack . show

wait = 3000000

dur :: Double
dur = 3000

animation :: Window -> TVar (ThreeModel a) -> JSM ()
animation w t = void $ requestAnimationFrame w =<< go
  where
    go = newRequestAnimationFrameCallback $ \(clock') -> do
      let clock = clock' - (wait / 1000)
      r <- go
      when (clock < dur) . void $ requestAnimationFrame w r

main :: IO ()
main = runJSorWarp 8080 $ do
  H.addInlineStyle $ decodeUtf8 $(embedFile "./assets/tailwind.min.css")
  H.addInlineStyle $ decodeUtf8 $(embedFile "./assets/style.css")
  let objF = grid3D 5 5 25
      --model = zip (repeat testText) (objF <$> [1..10])
  mod <- (defMod objF (take 250 $ repeat testText))
  model <- liftIO $ newTVarIO mod
  w <- currentWindowUnchecked
  _ <- (liftIO . forkIO $ threadDelay wait) >> animation w model
  shpadoinkle id runParDiff model (threeD) getBody


testText :: T.Text
testText = "Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of 'de Finibus Bonorum et Malorum' (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, 'Lorem ipsum dolor sit amet..', comes from a line in section 1.10.32."
