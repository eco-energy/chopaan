{-# LANGUAGE OverloadedStrings, TypeApplications, ScopedTypeVariables, OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes, ImpredicativeTypes, QuantifiedConstraints       #-}
{-# LANGUAGE DataKinds, GADTs                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules, TypeFamilies, NamedFieldPuns, TemplateHaskell, RecordWildCards, PackageImports #-}

module Chopaan.Ui.ThreeD where

import GHC.Generics hiding (R)
import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State

import Control.Concurrent.STM
import Data.Functor.Rep
import Data.Aeson
import System.IO (stderr, hPutStrLn, stdout, hFlush)

import qualified Data.Text as T
import           Data.FileEmbed              (embedFile)
import           Data.Text.Encoding          (decodeUtf8)

import UnliftIO.Concurrent (forkIO, threadDelay)
import Language.Javascript.JSaddle (ToJSVal(..), FromJSVal(..), valToObject)
import GHCJS.DOM (currentWindow, currentWindowUnchecked, currentDocumentUnchecked)
import "ghcjs-dom" GHCJS.DOM.Document (createElement, getDocumentElementUnchecked)
import GHCJS.DOM.Element (setId, getBoundingClientRect, getClientTop, getClientLeft, toElement, Element)
import GHCJS.DOM.NonElementParentNode (getElementById)
import GHCJS.DOM.Window (getInnerHeight, getInnerWidth, Window, requestAnimationFrame, getPageXOffset, getPageYOffset)
import GHCJS.DOM.DOMRectReadOnly (getTop, getWidth, getHeight, getLeft)
import GHCJS.DOM.RequestAnimationFrameCallback (newRequestAnimationFrameCallback, RequestAnimationFrameCallback)

import Shpadoinkle (Html, JSM, MonadJSM, liftJSM, TVar, shpadoinkle
                   , voidC, liftC', leftC', rightC', rightC, maybeC', liftCMay', eitherC'
                   , Continuation, pur, impur, kleisli, RawNode(..), RawEvent)
import Shpadoinkle.Run (runJSorWarp)
import qualified Shpadoinkle.Html as H
import Shpadoinkle.Html.Utils (getBody)
import Shpadoinkle.Widgets.Types (Humanize(..))
import Shpadoinkle.Backend.Snabbdom
import Shpadoinkle.Lens
import Shpadoinkle.Console
import Control.Lens hiding (simple, elements)
import Data.Generics.Product
import Data.Generics.Labels

import Linear.Vector
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Linear.Metric
import Linear.Projection

import qualified Chopaan.Ui.Style as Css
import Chopaan.Ui.Base
import Chopaan.Ui.Events
import Chopaan.Ui.Interaction

-- $ A Translation of
-- $ https://github.com/mrdoob/three.js/blob/dev/examples/jsm/renderers/CSS3DRenderer.js
-- $ https://github.com/mrdoob/three.js/blob/dev/src/core/Object3D.js
-- $ https://github.com/mrdoob/three.js/blob/dev/examples/jsm/controls/TrackballControls.js
-- $ https://github.com/mrdoob/three.js/blob/dev/examples/css3d_periodictable.html
default(T.Text)

data Camera = Camera
  { fov :: Double
  , aspect :: Double
  , near :: Double
  , far :: Double
  , matrixWorldInverse :: M44R
  , projectionTransform :: M44R
  , cameraObj :: Obj
  }
  deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

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

changeAspect :: Double -> Camera -> Camera
changeAspect a (c@Camera{..}) = c & #aspect .~ a
                     & (#projectionTransform) .~ (perspective fov aspect near far) 

transformCamera :: T -> Camera -> Camera
transformCamera t c = c & #cameraObj .~ (newO) 
                        & (#matrixWorldInverse) .~ (inv44 $ asT newO)
  where
    newO = transformObj t $ c ^. #cameraObj 

           
worldDirection :: Camera -> V3 R
worldDirection = normalize . (view (_xyz . column _z)) . matrixWorldInverse 

newtype Scene a = Scene { runScene :: [(a, Obj)] }
  deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

mkScene :: (Int -> Obj) -> [a] -> Scene a
mkScene objF = Scene . (flip zip (objF <$> [0,1..]))

transformScene :: T -> Scene a -> Scene a
transformScene p = Scene . fmap (second (transformObj p)) . runScene

data Screen = Screen
  { widthG :: Double
  , heightG :: Double
  , left :: Double
  , top :: Double
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

zeroScreen = Screen 0 0 0 0

data ThreeModel a = ThreeModel
  { scene :: Scene a
  , camera :: Camera
  , screen :: Screen
  } deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

transformModel :: T -> ThreeModel a -> ThreeModel a
transformModel p (ThreeModel s c x) = ThreeModel s c' x
  where
    c' = transformCamera p c


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


defCam :: V3 R -> Double -> Double -> Camera
defCam p w h = mkCam o 40 (w / h) 1 10000
  where
    o = mkObj p zero (V3 1 1 1) 


mkModel :: Screen -> (Int -> Obj) -> [a] -> (ThreeModel a)
mkModel s objF xs = ThreeModel (mkScene (objF' cam) xs) cam s
  where
    cam = (defCam camPos (widthG s) (heightG s))
    objF' c = (rotateObj (_rot . cameraObj $ c)) . objF
    camPos = V3 0 0 3000


getWH :: MonadJSM m => m (Double, Double)
getWH = do
  w <- currentWindowUnchecked
  height <- realToFrac <$> getInnerHeight w
  width <- realToFrac <$> getInnerWidth w
  return (width, height)

-- 
--

type ControlModel a = (ThreeModel a, Maybe Interact)

type Throttler m ev a = (H.Throttle m (ev -> JSM (Continuation m (ControlModel a))) (ControlModel a))

screenAspect :: Screen -> Double
screenAspect s = (widthG s / heightG s) 

getScreen :: Element -> JSM Screen
getScreen e = do
  win <- currentWindowUnchecked
  docE <- getDocumentElementUnchecked =<< currentDocumentUnchecked
  r <- getBoundingClientRect e
  debug @ToJSVal r
  (w, h) <- getWH
  --w <- getWidth r
  --h <- getHeight r
  t <- (\a b c -> a - c) <$> getTop r <*> getPageYOffset win <*> (getClientTop docE)
  l <- (\a b c -> a - c) <$> getLeft r <*> getPageXOffset win <*> (getClientLeft docE)
  return $ Screen w h l t



threeD :: forall m a. (MonadJSM m, Humanize a)
       -- => Throttler m Pointer a
       => ControlModel a
       -> Html m (ControlModel a)
threeD ((ThreeModel (Scene xs) c screen), track) = H.div rootProps [
  H.div (cameraCSS) $
    (\(x, y) -> H.div (objCSS y) . pure . H.text . humanize $ x) <$> xs
  ]
  where
    rootProps = rootHandler <> rootCSS
    rootHandler = [ H.listenRaw "resize" screenHandler
                  --, H.listenRaw "load" screenHandler
                  , rightC <$> onStart
                  , rightC <$> onEnd
                  , rightC <$> onMove
                  , rightC <$> onWheel
                  , voidC <$> noRightClick
                  ]
    screenHandler :: RawNode -> RawEvent -> JSM (Continuation m (ThreeModel a, x))
    screenHandler (RawNode n) re = do
      let cont = do
            e <- valToObject n
            debug @ToJSVal e
            newS <- getScreen =<< (fromJSValUnchecked @Element n) 
            return $ \(ThreeModel s c _) ->
                       (ThreeModel s (changeAspect (screenAspect newS) c) newS)
      return $ leftC' (impur (liftJSM cont))
    fov' = (c ^. #projectionTransform . _y . _y) * (heightG screen / 2)
    rootCSS = [ H.textProperty "id" "renderer"
              , styleP "overflow:hidden"
              , styleP ("perspective:" <> (textS fov') <> "px")
              , H.class' Css.w_screen
              , H.class' Css.h_screen
              , styleP "background-color: black"
              , styleP "touch-action: none"
              ]
    cameraCSS =
      [ H.textProperty "id" "camera"
      , transformP $ (cameraCSSMat c) <> (translatePx (widthG screen / 2) (heightG screen / 2))
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
               , styleP "opacity:0.9"
               ]


grid3D :: Int -> Int -> Int -> (Int -> Obj)
grid3D row col stack = (gridPos)
  where
    gridPos :: Int -> Obj
    gridPos i = translateObj (points !! i) zeroObj
    points = [V3 x y z | z <- zs, y <- ys, x <- xs]
    xs = [(-2000), (-2000 + elWidth + widthOffset)..2000]
    ys = [(-2000), (-2000 + elHeight + heightOffset)..2000]
    -- Depth is infinite. So the zip must provide a surface for all depths
    zs = [-800, ((-800) - (elDepth + depthOffset))..]
    elWidth = 400
    widthOffset = 800
    elDepth = 200
    depthOffset = 200
    elHeight = 400
    heightOffset = 800

deltaModel :: Interact -> ThreeModel a -> ThreeModel a
deltaModel del = transformModel $ evalI del

styleP :: T.Text -> (T.Text, H.Prop m a)
styleP = H.textProperty "style"

transformP :: T.Text -> (T.Text, H.Prop m a)
transformP x = styleP $ "transform:" <> x

translatePx :: R -> R -> T.Text
translatePx w h = "translate(" <> (toPx w) <> "," <> (toPx h) <> ")"

toPx :: Show a => a -> T.Text
toPx = (<> "px") . textS

textS :: Show a => a -> T.Text
textS = T.pack . show


wait = 3000000

dur :: Double
dur = 3000

animation :: Window -> TVar (ControlModel a) -> JSM (RequestAnimationFrameCallback)
animation w tv = go
  where
    go = newRequestAnimationFrameCallback $ \(clock') -> () <$ do
      (threeM, t) <- liftIO . atomically $ readTVar tv
      case t of
        Nothing -> return ()
        Just ts -> do
          debug @ToJSON ts
          liftIO . atomically $ do
            let
              m' = deltaModel ts threeM
              b' = (buttonMap $ getButton ts)
              t' = case b' of
                Nothing -> Nothing
                (Just f) -> Just (f $ getState ts)
            writeTVar tv (m', t')
      (requestAnimationFrame w) =<< (animation w tv)


threeDM :: (Eq a, NFData a, ToJSON a, Humanize a) => (Int -> Obj) -> [a] -> JSM RawNode
threeDM objF xs = do
  let vId = "threeDView"
  doc <- currentDocumentUnchecked
  isSubsequent <- traverse toJSVal =<< getElementById doc vId
  case isSubsequent of
    Just raw -> return $ RawNode raw
    Nothing -> do
      --throt <- liftIO $ H.throttle 1
      win <- currentWindowUnchecked
      elm <- createElement doc "div"
      setId elm vId
      debug @ToJSVal elm
      (w, h) <- getWH
      let mod = mkModel (Screen w h 0 h) objF xs 
      model <- liftIO $ newTVarIO (mod, Nothing)
      _ <- requestAnimationFrame win =<< animation win model
      raw <- RawNode <$> toJSVal elm
      ctx <- askJSM
      _ <- forkIO $ threadDelay 10
           >> shpadoinkle id runSnabbdom model (threeD . trapper @ToJSON ctx) (pure raw)
      return raw


main :: IO ()
main = runJSorWarp 8080 $ do
  H.addInlineStyle $ decodeUtf8 $(embedFile "./assets/tailwind.min.css")
  H.addInlineStyle $ decodeUtf8 $(embedFile "./assets/style.css")
  win <- currentWindowUnchecked
  --throt <- liftIO $ H.throttle 1
  scr <- (\x -> (getScreen x))
         -- =<< (\x -> (debug @ToJSVal x >> (fromJSValUnchecked @Element) x))
         =<< (getDocumentElementUnchecked =<< currentDocumentUnchecked)
  debug @ToJSON scr
  let objF = grid3D 5 5 25
  let mod = mkModel scr objF (take 100 $ repeat testText) 
  model <- liftIO $ newTVarIO (mod, Nothing)
  _ <- requestAnimationFrame win =<< animation win model
  ctx <- askJSM
  shpadoinkle id runSnabbdom model (threeD . trapper @ToJSON ctx) (getBody)
--  . trapper @ToJSON ctx

testText :: T.Text
testText = "Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of 'de Finibus Bonorum et Malorum' (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, 'Lorem ipsum dolor sit amet..', comes from a line in section 1.10.32."
