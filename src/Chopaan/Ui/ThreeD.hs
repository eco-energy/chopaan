{-# LANGUAGE OverloadedStrings, TypeApplications, ScopedTypeVariables, OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules, TypeFamilies, NamedFieldPuns, TemplateHaskell, RecordWildCards, PackageImports #-}

module Chopaan.Ui.ThreeD where

import GHC.Generics hiding (R)
import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class


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
                   , voidC, liftC', leftC', rightC', maybeC', liftCMay', eitherC'
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
import Chopaan.Ui.Interaction

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


transformObj :: M44R -> Obj -> Obj
transformObj t o = o
                   & #_pos %~ ((t ^. translation) ^+^) 
                   & (#_localTransform) %~ (t !*!)
                   & (#_worldTransform) %~ (t !*!)
                   

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

changeAspect :: Double -> Camera -> Camera
changeAspect a (c@Camera{..}) = c & #aspect .~ a
                     & (#projectionTransform) .~ (perspective fov aspect near far) 

transformCamera :: M44R -> Camera -> Camera
transformCamera t c = c & #cameraObj .~ (newO) 
                        & (#matrixWorldInverse) .~ (inv44 $ asT newO)
  where
    newO = transformObj t $ c ^. #cameraObj 
                      
worldDirection :: Camera -> V3 R
worldDirection = normalize . (view (_xyz . column _z)) . matrixWorldInverse 

data Scene a = Scene [(a, Obj)]
  deriving (Eq, Show, Generic, NFData)

mkScene :: (Int -> Obj) -> [a] -> Scene a
mkScene objF = Scene . (flip zip (objF <$> [0,1..]))

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

type ControlModel a = (ThreeModel a, Maybe TrackballState)

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

threeD :: forall m a. (MonadJSM m, Humanize a) => ControlModel a -> Html m (ControlModel a)
threeD ((ThreeModel (Scene xs) c screen), track) = H.div rootProps [
  H.div (cameraCSS) $
    (\(x, y) -> H.div (objCSS y) . pure . H.text . humanize $ x) <$> xs
  ]
  where
    rootProps = rootHandler <> rootCSS
    rootHandler = [ H.listenRaw "resize" screenHandler
                  --, H.listenRaw "load" screenHandler
                  , H.onResizeC 
                    (impur $ do
                                 debug @ToJSON "On Resize Called"               
                                 return id)
                  , onPointerDown (pure . rightC' . startAction)
                  , onPointerMove (pure . rightC' . maybeC' . controlTf)
                  , onPointerUp (pure . rightC' . endAction)
                  ]
    screenHandler :: RawNode -> RawEvent -> JSM (Continuation m (ThreeModel a, x))
    screenHandler (RawNode n) re = do
      let cont = do
            e <- valToObject n
            debug @ToJSVal e
            newS <- getScreen =<< (fromJSValUnchecked @Element n) 
            return $ \(ThreeModel s c _) -> (ThreeModel s (changeAspect (screenAspect newS) c) newS)
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


data TrackballAction = Pan | Zoom | Rotate 
  deriving (Eq, Ord, Show, Generic, NFData)

type ActionStates = Either ZoomState (Either PanState RotState)
type ZoomState = V2 R
type PanState = (V2 R, V2 R)
type RotState = (V2 R, V2 R)


liftZ :: (Functor m) => Continuation m ZoomState -> Continuation m (ZoomState, M44R)
liftZ = liftC' (\z (z', t) -> (z, over translation (^+^ (baseScale ^* (zoomFactor . zdiff $ (z, z')))) t)) (\(z, t) -> z)
  where
    baseScale = (V3 1 1 1)
    zoomFactor x = 1.0 + x * 1.2
    zdiff (start, end) = end ^. _y - start ^. _y 
    
liftP :: Continuation m PanState -> Continuation m (PanState, M44R)
liftP = undefined

liftR :: Continuation m RotState -> Continuation m (RotState, M44R)
liftR = undefined


zoomA :: PointerEv -> Continuation m ZoomState
zoomA PointerEv{pos} = pur $ \endP -> V2 (fst pos) (snd pos) ^-^ endP

panA :: PointerEv -> Continuation m PanState
panA = undefined

rotateA :: PointerEv -> Continuation m RotState
rotateA = undefined




getAct :: (Applicative m) => TrackballAction -> (PointerEv -> Continuation m (ActionStates, M44R))
getAct Pan = liftCMay' toAct mayPan . (liftP . panA)
  where
    toAct :: (PanState, M44R) -> (ActionStates, M44R) -> (ActionStates, M44R)
    toAct a b = (Right . Left . fst $ a, snd a !*! snd b)
    mayPan (Right (Left k), a) = Just (k, a)
    mayPan _ = Nothing
getAct Zoom = liftCMay' toAct mayZoom . liftZ . zoomA
  where
    toAct :: (ZoomState, M44R) -> (ActionStates, M44R) -> (ActionStates, M44R)
    toAct a b = (Left . fst $ a, snd a !*! snd b)
    mayZoom (Left k, a) = Just (k, a)
    mayZoom _ = Nothing
getAct Rotate = liftCMay' toAct mayRot . liftR . rotateA
  where
    toAct :: (RotState, M44R) -> (ActionStates, M44R) -> (ActionStates, M44R)
    toAct a b = (Right . Right . fst $ a, snd b !*! snd a)
    mayRot (Right (Right k), a) = Just (k, a)
    mayRot _ = Nothing

type TrackballState = (Maybe TrackballAction, (ActionStates, M44R))

defActState = (Just Zoom, (Left zero, identity))

getActionType :: Button -> TrackballAction
getActionType = const Zoom

startAction :: PointerEv -> Continuation m (Maybe TrackballState) 
startAction PointerEv{button, pos} = pur $ (\_ -> Just (getActionType <$> button, (Left $ V2 (fst pos) (snd pos), identity)))

endAction :: PointerEv -> Continuation m (Maybe TrackballState)
endAction _ = pur (const Nothing)
                      
controlTf :: Applicative m => PointerEv -> Continuation m (TrackballState)
controlTf = rightC' . (getAct Zoom)

deltaModel :: TrackballState -> ThreeModel a -> ThreeModel a
deltaModel (_, (_, diff)) m = over #camera (transformCamera diff) m

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
      liftIO . atomically $ do
        (threeM, t) <- readTVar tv
        case t of
          Nothing -> return ()
          Just ts -> writeTVar tv (deltaModel ts threeM, Just ts)
      (requestAnimationFrame w) =<< (animation w tv)

threeDM :: (Eq a, NFData a, Humanize a) => (Int -> Obj) -> [a] -> JSM RawNode
threeDM objF xs = do
  let vId = "threeDView"
  doc <- currentDocumentUnchecked
  isSubsequent <- traverse toJSVal =<< getElementById doc vId
  case isSubsequent of
    Just raw -> return $ RawNode raw
    Nothing -> do
      win <- currentWindowUnchecked
      elm <- createElement doc "div"
      setId elm vId
      (w, h) <- getWH
      let mod = mkModel (Screen w h 0 h) objF xs 
      model <- liftIO $ newTVarIO (mod, Nothing)
      _ <- requestAnimationFrame win =<< animation win model
      raw <- RawNode <$> toJSVal elm
      _ <- forkIO $ shpadoinkle id runSnabbdom model threeD (pure raw)
      return raw


main :: IO ()
main = runJSorWarp 8080 $ do
  H.addInlineStyle $ decodeUtf8 $(embedFile "./assets/tailwind.min.css")
  H.addInlineStyle $ decodeUtf8 $(embedFile "./assets/style.css")
  win <- currentWindowUnchecked
  scr <- (\x -> (debug @ToJSVal x >> getScreen x))
         -- =<< (\x -> (debug @ToJSVal x >> (fromJSValUnchecked @Element) x))
         =<< (getDocumentElementUnchecked =<< currentDocumentUnchecked)
  debug @ToJSON scr
  let objF = grid3D 5 5 25
  let mod = mkModel scr objF (take 1000 $ repeat testText) 
  model <- liftIO $ newTVarIO (mod, Nothing)
  _ <- requestAnimationFrame win =<< animation win model
  shpadoinkle id runSnabbdom model (threeD) (getBody)


testText :: T.Text
testText = "Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of 'de Finibus Bonorum et Malorum' (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, 'Lorem ipsum dolor sit amet..', comes from a line in section 1.10.32."
