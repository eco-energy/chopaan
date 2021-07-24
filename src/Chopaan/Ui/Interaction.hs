{-# Language OverloadedStrings, DeriveGeneric, DeriveAnyClass, TypeApplications #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes, ImpredicativeTypes, QuantifiedConstraints       #-}
{-# LANGUAGE DataKinds, GADTs                 #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedLabels, NamedFieldPuns     #-}
{-# LANGUAGE FlexibleContexts, ExtendedDefaultRules          #-}

module Chopaan.Ui.Interaction where

import Prelude hiding (interact)
import GHC.Generics hiding (R)
import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Lens

import qualified Data.Text as T
import Data.Generics.Labels
import Data.Generics.Sum
import Data.Maybe

import           GHCJS.DOM.Types              hiding (Text, Touch)
import           Language.Javascript.JSaddle  hiding (JSM, liftJSM, toJSString)

import           Shpadoinkle ( listenRaw, Continuation(..), RawEvent(..), Prop
                             , MonadUnliftIO, JSM, MonadJSM, liftJSM
                             )
import Shpadoinkle ( TVar, shpadoinkle
                   , voidC, liftC', leftC', rightC', maybeC', comaybeC', liftCMay', eitherC'
                   , Continuation, pur, impur, kleisli, merge
                   , writeUpdate, shouldUpdate, constUpdate
                   )
import           Shpadoinkle.Console
import           Shpadoinkle.Html ( debounce, Debounce(..)
                                  , throttle, Throttle (..)
                                  , preventDefault
                                  )

import Data.Monoid

import Linear.Affine
import Linear.Vector
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Linear.Metric
import Linear.Projection

import Chopaan.Ui.Base


default(T.Text)

newtype Ctrl a = Ctrl { unCtrl :: (TVar [Interact a]) }
  deriving (Generic)

type CurPos = Point V2 R
type LastPos = Point V2 R

type PosDiff = Diff (Point V2) R

type PosState = (LastPos, PosDiff)

initPosState :: CurPos -> PosState
initPosState p = (p, zero)

diffPos :: CurPos -> Unop PosState
diffPos p = \(e, _) -> (p, (p .-. e))

runCtrl :: (MonadUnliftIO m, Applicative m) => Ctrl -> Continuation m (Maybe Interact) -> m ()
runCtrl (Ctrl t) c = writeUpdate t (listC c)
  where
    listC :: (Applicative m) => Continuation m (Maybe a) -> Continuation m [a]
    listC = liftCMay' (\x a -> a
                        <> (fromMaybe mempty (fmap pure x)))
            (\(a:_) -> pure $ pure a)

onStart :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
onStart = onPointerDown (pure . startControl)

onEnd :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
onEnd = onPointerUp (pure . endControl)

boopZ :: (Applicative m) => Wheel -> Continuation m (Maybe Interact)
boopZ = maybeC' . (boop' zoomI (preview #_ZoomI) . zoomC)


onWheel :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
onWheel = onEv "wheel" toWheel (pure <$> boopZ)

--onEnter

onExit :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
onExit = onPointer "exit" (pure . endControl)

move :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
move = onPointerMove (pure . controlC)

startControl :: (Applicative m) => Pointer -> Continuation m (Maybe Interact)
startControl p = pur getInitState
  where
    getInitState :: Maybe Interact -> Maybe (Interact)
    getInitState prev = case button p of
      NoneB -> case prev of
        Nothing -> Nothing
        (Just (ZoomI z)) -> Just . zoomI $ zoomA' (pos p) z
        (Just (PanI z)) -> Just . panI $ panA (pos p) z
        (Just (RotateI z)) -> Just . rotateI $ rotateA (pos p) z
        (Just (ProdI z)) -> error "How should we treat Tr?" --Just . zoomI $ zoomA (pos p) z
      ZoomB -> Just . zoomI $ initZoomState (pos p)
      PanB -> Just . panI $ initPanState (pos p)
      RotateB -> Just . rotateI $ initRotateState (pos p)
      TouchRotateB -> error "touch rotate not handled"
      TouchZoomB -> error "touch zoom not handled"
      

controlC :: (Applicative m) => Pointer -> Continuation m (Maybe Interact)
controlC = maybeC' . control'

endControl :: (Applicative m) => a -> Continuation m (Maybe Interact)
endControl = const . pur . const $ Nothing


control' :: (Applicative m) => Pointer -> Continuation m Interact
control' p = (boop' panI (preview #_PanI) $ panC p)
            <> (boop' rotateI (preview #_RotateI) $ rotateC p)


data PointerType = Touch | Mouse
  deriving (Eq, Ord, Show, Generic, Enum, Bounded, ToJSON, NFData)

getPointerType :: JSString -> Maybe PointerType
getPointerType "touch" = Just Touch
getPointerType "mouse" = Just Mouse
getPointerType _ = Nothing



data Pointer = Pointer
  { pos :: CurPos
  , pointerType :: Maybe PointerType
  , pointerId :: Int
  , button :: Button
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON)


toPos :: (R, R) -> Point V2 R
toPos = (zero .+^) . (uncurry V2)


toPointer :: RawEvent -> JSM (Maybe Pointer)
toPointer (RawEvent e') = liftJSM $ do
  preventDefault (RawEvent e')
  e <- valToObject e'
  pt <- valToStr =<< getProp "pointerType" e
  x <- valToNumber =<< getProp "pageX" e
  y <- valToNumber =<< getProp "pageY" e
  pid <- fmap round $ valToNumber =<< getProp "pointerId" e
  b <- fmap (round) $ valToNumber =<< getProp "button" e
  let b' = toEnum (b + 1)
  return $ Just $ Pointer (toPos (x, y)) (getPointerType pt) pid b'


onEv :: forall m i o. T.Text -> (RawEvent -> (JSM (Maybe i))) -> (i -> JSM (Continuation m o)) -> (T.Text, Prop m o)
onEv ev parse f = listenRaw ev (\_ e ->  (step =<< parse e))
  where
    step (Just x) = f x
    step Nothing = pure . pur $ id

onPointer :: T.Text -> (Pointer -> JSM (Continuation m a)) -> (T.Text, Prop m a)
onPointer act = onEv ("pointer" <> act) toPointer     

onPointerUp ::  (Pointer -> JSM (Continuation m a)) -> (T.Text, Prop m a)
onPointerUp = onPointer "up"
onPointerDown :: (Pointer -> JSM (Continuation m a)) -> (T.Text, Prop m a)
onPointerDown = onPointer "down"
onPointerMove :: (Pointer -> JSM (Continuation m a)) -> (T.Text, Prop m a)
onPointerMove = onPointer "move"


onMove :: Throttle m (Pointer -> JSM (Continuation m a)) a
       -> (Pointer -> JSM (Continuation m a))
       -> (T.Text, Prop m a)
onMove t = runThrottle t onPointerMove

--onPointerUp = listenRaw "pointerup"

unitV :: V3R
unitV = V3 1 1 1


type Eye = V3 R
type UpDir = V3 R
type PanSpeed = R
type PanS = (R, Eye, PosState, UpDir, V3 R) -- (translate in the x and y planes)

initPanState :: CurPos -> PanS
initPanState p = (1.0, zero, initPosState p, V3 0 1 0, zero)

setLength s = (^* s) . normalize

data CState = CState
  { eye :: V3R
  , lastAxis :: V3 R
  , zoomS :: (LastPos, CurPos)
  , panS :: (LastPos, CurPos)
  , pointers :: [PointerType]
  } deriving (Eq, Ord, Show, Generic, NFData)


panT :: PanS -> T
panT (panSpeed, eye, (lastP, pd), upDir, panDiff) = f 
  where
    f =  if ((norm pd) > 0)
               then Endo $ (over translation (.+^ panDiff))
               else Endo id

panI :: PanS -> Interact
panI = PanI

panA :: CurPos -> Unop PanS
panA p (panSpeed, eye, (lastP, pd), upDir, _) = (panSpeed, eye, (p, pd'), upDir, newP) 
  where
    pd' = p .-. lastP
    scaledChange = pd' ^* ((norm eye) * panSpeed)
    newP = (setLength (scaledChange ^. _x) (cross eye upDir)) .+^ (setLength (scaledChange ^. _y) upDir)

panC :: Pointer -> Continuation m (PanS)
panC Pointer{pos, button} = case button of
  PanB -> pur (panA pos)
  _ -> pur id

type ZoomS = PosState

initZoomState :: CurPos -> ZoomS
initZoomState = initPosState

data DeltaUnit = PixelDelta | LineDelta | PageDelta
  deriving (Eq, Ord, Show, Generic, Enum, Bounded, NFData, ToJSON)

data Wheel = Wheel
  { deltaUnit :: DeltaUnit
  , wheelDelta :: V2 R
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON)

toWheel :: RawEvent -> JSM (Maybe Wheel)
toWheel (RawEvent e') = liftJSM $ do
  preventDefault (RawEvent e')
  e <- valToObject e'
  debug @ToJSVal e
  del <- (V2 <$> (numVal "deltaX" e) <*> (numVal "deltaY" e)) -- <*> (numVal "deltaZ" e))
  b <- fmap (toEnum . round) $ valToNumber =<< getProp "deltaMode" e
  return $ Just $ Wheel b del
  where
    numVal p = valToNumber <=< (getProp p)

zoomT :: ZoomS -> T
zoomT z = Endo $ over translation (^+^ (unitV ^* (zoomFactor . ydiff $ z)))
  where
    zoomFactor x = 1.0 + (x * 0.1)
    ydiff (_, diff) = diff ^. _y

zoomA :: DeltaUnit -> V2 R -> Unop ZoomS
zoomA d p (lastP, pDiff) = (lastP .+^ p, p)

zoomA' :: CurPos -> Unop ZoomS
zoomA' p (lastP, pDiff) = (p, pDiff .+^ (p .-. lastP))

zoomI :: ZoomS -> Interact
zoomI = ZoomI

zoomC :: Wheel -> Continuation m (ZoomS)
zoomC Wheel{deltaUnit, wheelDelta} = pur (zoomA deltaUnit wheelDelta)


data RotateS = RotateS
  { rotationS :: QuatR
  , rotPosDiff :: (LastPos, PosDiff)
  , eyeDirection :: V3R
  , upDir :: V3R
  , sideDir :: V3R
  , moveDir :: V3R
  , rotAxis :: V3R
  , rotSpeed :: R
  }
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON)

initRotateState :: CurPos -> RotateS
initRotateState p = RotateS defQuat (initPosState p) zero zero zero zero zero 0.1

rotateT :: RotateS -> T
rotateT r = Endo $ \m -> (mkTransformation (rotationS r) zero) !*! m

rotateI :: RotateS -> Interact
rotateI = RotateI

rotateA :: CurPos -> Unop RotateS
rotateA p RotateS{rotationS, rotPosDiff, eyeDirection, upDir, sideDir, moveDir, rotAxis, rotSpeed} = r'
  where
    r' = RotateS q' pd' ed' ud' sd' md' ax' rotSpeed
    q' = axisAngle ax' ang
      where
        ang =  norm md'
    pd' = (p, p .-. lastPos)
      where
        (lastPos, posDiff) = rotPosDiff
    ed' = normalize ed
      where ed = eyeDirection -- (scope.object.position - scope.target)
    ud' = setLength (pd' ^. _2 . _y) $ normalize ud
      where ud = upDir -- scope.object.up
    sd' = setLength (pd' ^. _2 . _y) $ cross ud' ed'
    md' = (V3 (pd' ^. _2 . _x) (pd' ^. _2 . _y) 0)
    ax' = cross md' eyeDirection

  
rotateC :: Pointer -> Continuation m (RotateS)
rotateC Pointer{pos, button} = case button of
  RotateB -> pur (rotateA pos)
  _ -> pur id


type InteractM m a = (StateT (Interact) m)

data Interact = ZoomI ZoomS
              | PanI PanS
              | RotateI RotateS
              | Tr M44R
              deriving (Eq, Ord, Show, Generic, NFData, ToJSON)

boop :: Unop a -> Continuation m a
boop = pur

evalI :: Interact -> T
evalI (ZoomI z) = zoomT z
evalI (PanI p) = panT p
evalI (RotateI r) = rotateT r
evalI (Tr i) = Endo (i !*!)

data Button = NoneB | RotateB | ZoomB | PanB | TouchRotateB | TouchZoomB
  deriving (Eq, Ord, Show, Generic, Bounded, Enum, ToJSON, NFData)

boop' :: Applicative m => (a -> Interact) -> (Interact -> Maybe a) -> Continuation m a -> Continuation m Interact
boop' f g = liftCMay' (mergeI . f) g

mergeI :: Interact -> Interact -> Interact
mergeI a b = Tr $ (appEndo $ (evalI a) <> (evalI b)) identity

act :: Obj -> T -> Obj
act = flip transformObj 
