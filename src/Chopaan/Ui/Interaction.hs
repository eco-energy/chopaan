{-# Language OverloadedStrings, DeriveGeneric, DeriveAnyClass, TypeApplications #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes, ImpredicativeTypes, QuantifiedConstraints       #-}
{-# LANGUAGE DataKinds, GADTs                 #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedLabels, NamedFieldPuns     #-}
{-# LANGUAGE FlexibleContexts, ExtendedDefaultRules, LambdaCase          #-}

module Chopaan.Ui.Interaction where

import Prelude hiding (interact)
import GHC.Generics hiding (R)
import Control.DeepSeq
import Control.Monad ()
import Control.Monad.IO.Class ()
import Control.Monad.Trans.State
import Control.Lens

import qualified Data.Text as T
import Data.Aeson
import Data.Generics.Labels ()
import Data.Generics.Sum ()
import Data.Maybe


import           Shpadoinkle ( Continuation(..), Prop
                             , MonadUnliftIO, JSM
                             )
import Shpadoinkle ( TVar, shpadoinkle
                   , voidC, liftC', leftC', rightC', maybeC', comaybeC', liftCMay', eitherC'
                   , Continuation, pur, impur, kleisli, merge
                   , writeUpdate, shouldUpdate, constUpdate
                   , before, after
                   )
import           Shpadoinkle.Console
import           Shpadoinkle.Html (Throttle(..))
import Shpadoinkle.Lens
import Data.Monoid

import Linear.Affine
import Linear.Vector
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4 (vector, point)
import Linear.Quaternion
import Linear.Metric
import Linear.Projection

import Chopaan.Ui.Base
import Chopaan.Ui.Events

default(T.Text)

type Ctrl = TVar [Interact]

runCtrl :: (MonadUnliftIO m, Applicative m) => Ctrl -> Continuation m (Maybe Interact) -> m ()
runCtrl t c = writeUpdate t (listC c)
  where
    listC :: (Applicative m) => Continuation m (Maybe a) -> Continuation m [a]
    listC = liftCMay' (\x a -> a
                        <> (fromMaybe mempty (fmap pure x)))
            (\(a:_) -> pure $ pure a)



type PosDiff = Diff (Point V2) R

type PosState = (LastPos, PosDiff)


initPosState :: CurPos -> PosState
initPosState p = (p, zero)

diffPos :: CurPos -> Unop PosState
diffPos p (lastP, pDiff) = (p, pDiff .+^ (p .-. lastP))

diffWheel :: DeltaUnit -> V2 R -> Unop PosDiff
diffWheel d p pDiff = pDiff ^+^ p' -- sub because d is in -ve y-axis
  where
    p' = case d of
      PixelDelta -> p ^* 0.00025
      LineDelta -> p ^* 0.01
      PageDelta -> p ^* 0.025

onStart :: (Applicative m) => Screen -> (T.Text, Prop m (Maybe Interact))
onStart s = onPointerDown s (pure . startP)

onEnd :: (Applicative m) => Screen -> (T.Text, Prop m (Maybe Interact))
onEnd s = onPointerUp s (pure . endP)

onWheel :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
onWheel = onEv "wheel" toWheel (pure . wheelControl')

onExit :: (Applicative m) => Screen -> (T.Text, Prop m (Maybe Interact))
onExit s = onPointer s "exit" (pure . endP)

onMove :: (Applicative m) => Screen -> (T.Text, Prop m (Maybe Interact))
onMove s = onPointerMove s (pure . maybeC' . pointerControl)

startP :: (Applicative m) => Pointer -> Continuation m (Maybe Interact)
startP p = pur getInitState
  where
    getInitState :: Maybe Interact -> Maybe (Interact)
    getInitState _ = case button p of
      ZoomB -> Just . zoomI $ initPosState (pos p)
      PanB -> Just . panI $ initPosState (pos p)
      RotateB -> Just . rotateI $ initPosState (pos p)
      _ -> Nothing


endP :: (Applicative m) => a -> Continuation m (Maybe Interact)
endP = const . pur . const $ Nothing

toInteract :: (Maybe Button, PosState) -> Interact -> Interact
toInteract (Nothing, _) = id 
toInteract (Just b, s) = case buttonMap b of
  Nothing -> id
  Just c -> const (c s) --(getState p)))

fromInteract :: Interact -> (Maybe Button, PosState)
fromInteract (ZoomI p) = (Just ZoomB, p)
fromInteract (PanI p) = (Just PanB, p)
fromInteract (RotateI p) = (Just RotateB, p)
fromInteract (Tr _ j) = fromInteract j

boop :: (Applicative m) => Continuation m (Maybe Button, PosState) -> Continuation m Interact
boop = liftC' toInteract fromInteract


-- $ This is a continuation that looks at the pointer event and decides what the next
-- $ state should be.
-- $ The structure of events is such that the initial event is tagged with the correct
-- $ button but all its subsequent movement has a button type of NoneB.
-- $ No button at all means the last state was the initial state, or that the button
-- $ parse failed in the event decoding.
-- $ We want events that are tagged with NoneB to be re-tagged if they're preceeded
-- $ by another valid button, in a way that identifies the entire continuation
-- $ as a Pan or a Rotation.
-- $ The state should be re-initalized if the event type changes.
pointerC :: (Applicative m) => Pointer -> Continuation m (Maybe Button, PosState)
pointerC Pointer{pos, button} = case button of
  RotateB -> interactionC
  ZoomB -> interactionC
  PanB -> interactionC
  NoneB -> pur (rememberLast)
  _ -> pur id
  where
    interactionC = pur onChangeReInit
      where
        onChangeReInit (Nothing, s) = (Just button, diffPos pos s)
        onChangeReInit (Just b, s) = case (b == button) of
          True -> (Just button, diffPos pos s)
          False -> (Just button, diffPos pos (initPosState pos))
    rememberLast (Nothing, s) = (Nothing, s)
    rememberLast (Just NoneB, s) = (Nothing, s)
    rememberLast (Just ZoomB, s) = (Just ZoomB, diffPos pos s)
    rememberLast (Just PanB, s) = (Just PanB, diffPos pos s)
    rememberLast (Just RotateB, s) = (Just RotateB, diffPos pos s)
    rememberLast (Just _, _) = error "Not Handled"

pointerControl :: (Applicative m) => Pointer -> Continuation m Interact
pointerControl p = boop (pointerC p)

wheelC :: forall m. (Applicative m) => Wheel -> Continuation m (Maybe Button, PosState)
wheelC w = ((generalize _1 (pur $ const (Just ZoomB))) <> (generalize _2 $ pos w))
  where
    pos :: Wheel -> Continuation m PosState
    pos Wheel{deltaUnit, wheelDelta} = rightC' $ pur (diffWheel deltaUnit wheelDelta)


wheelControl :: (Applicative m) => Wheel -> Continuation m (Interact)
wheelControl = boop . wheelC


wheelControl' :: (Applicative m) => Wheel -> Continuation m (Maybe Interact)
wheelControl' = withInitC z0 . wheelControl
  where
    z0 = zoomI . initPosState $ pure 0
    -- maybeC' has wrong semantics here.
    -- We need withInitC to always start this continuation with our initial zoom value. So we use before to conditionally initialize the state
    withInitC a = (before (pur onMay)) . maybeC'
      where
        onMay (Just (ZoomI s)) = (Just $ ZoomI s)
        onMay _ = Just a
    


-- onTMove :: Throttle m (Pointer -> JSM (Continuation m a)) a
--        -> (Pointer -> JSM (Continuation m a))
--        -> (T.Text, Prop m a)
-- onTMove t = runThrottle t onPointerMove

--onPointerUp = listenRaw "pointerup"

unitV :: V3R
unitV = V3 1 1 1


type Eye = V3 R
type UpDir = V3 R


scaleNorm s = (^* s) -- . normalize

panT :: PosState -> T
panT (_, delta) = Endo $ \m -> (diffMat m)
  where
    diffMat m = (over translation (^+^ (panDiff pos up delta)) m)
      where
        pos = negated $ m ^. _m33 . _z
        up = m ^. _m33 . _y

panDiff :: V3 R -> V3 R -> V2 R -> V3 R
panDiff pos up delta = (scaleNorm (scaledChange ^. _x) (cross pos up))--
                ^+^ (scaleNorm (scaledChange ^. _y) up)
  where
    panSpeed = 1.0
    scaledChange = delta ^* ((norm pos) * panSpeed)


zoomT :: PosState -> T
zoomT z = Endo $ \m -> scaled (point $ unitV ^* (zoomFactor (z ^. _2 . _y))) !*! m
  where
    zoomFactor x = exp (x * zoomSpeed)
    zoomSpeed = 1.2



rotateT :: PosState -> T
rotateT (_, delta) = Endo $ \m -> (q' m) !*! m
  where
    q' m = mkTransformationMat (fromQuaternion (rot ed ud delta)) (zero)
      where
        ud = m ^. _m33 . _y
        ed = negate $ m ^. _m33 . _z

rot :: V3 R -> V3 R -> V2 R -> QuatR
rot pos up delta = axisAngle ax ang
  where
    ang =  norm (V3 (delta ^. _x) (delta ^. _y) 0)
    ud' = scaleNorm (delta ^. _y) up -- upDir 
    sd' = scaleNorm (delta ^. _x) $ cross ud' pos
    md' = (ud' ^+^ sd')--
    ax = cross md' sd'



type InteractM m a = (StateT (Interact) m)

data Interact = ZoomI PosState
              | PanI PosState
              | RotateI PosState
              | Tr Interact Interact
              deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

zoomI :: PosState -> Interact
zoomI = ZoomI

panI :: PosState -> Interact
panI = PanI

rotateI :: PosState -> Interact
rotateI = RotateI

trI :: Interact -> Interact -> Interact
trI = Tr

getState :: Interact -> PosState
getState (ZoomI p) = p
getState (PanI p) = p
getState (RotateI p) = p
getState (Tr _ j) = getState j

getButton :: Interact -> Button
getButton (ZoomI _) = ZoomB
getButton (PanI _) = PanB
getButton (RotateI _) = RotateB
getButton (Tr _ j) = (getButton j)

evalI :: Interact -> T
evalI (ZoomI z) = zoomT z
evalI (PanI p) = panT p
evalI (RotateI r) = rotateT r
evalI (Tr i j) = (evalI i) <> (evalI j)

getT :: Interact -> M44R
getT t = appEndo (evalI t) identity


buttonMap :: Button -> Maybe (PosState -> Interact)
buttonMap = \case
  NoneB -> Nothing
  ZoomB -> Just zoomI
  PanB -> Just panI
  RotateB -> Just rotateI
  TouchRotateB -> error "touch rotate not handled"
  TouchZoomB -> error "touch zoom not handled"


act :: Obj -> T -> Obj
act = flip transformObj
