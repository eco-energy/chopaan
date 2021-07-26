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

diffWheel :: DeltaUnit -> V2 R -> Unop PosState
diffWheel d p (lastP, pDiff) = (lastP .+^ p, p .+^ pDiff)

onStart :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
onStart = onPointerDown (pure . startP)

onEnd :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
onEnd = onPointerUp (pure . endP)

onWheel :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
onWheel = onEv "wheel" toWheel (pure . wheelControl')

onExit :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
onExit = onPointer "exit" (pure . endP)

onMove :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
onMove = onPointerMove (pure . maybeC' . pointerControl)

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

pointerC :: (Applicative m) => Pointer -> Continuation m (Maybe Button, PosState)
pointerC Pointer{pos, button} = case button of
  RotateB -> interactionC
  ZoomB -> interactionC
  PanB -> interactionC
  NoneB -> pur (rememberLast)
  _ -> pur id
  where
    interactionC = (generalize _1 $ pur (const (Just button)))
      <> (generalize _2 $ pur (diffPos pos))
    rememberLast (Nothing, s) = (Nothing, s)
    rememberLast (Just NoneB, s) = (Nothing, s)
    rememberLast (Just ZoomB, s) = (Just ZoomB, diffPos pos s)
    rememberLast (Just PanB, s) = (Just PanB, diffPos pos s)
    rememberLast (Just RotateB, s) = (Just RotateB, diffPos pos s)
    rememberLast (Just _, _) = error "Not Handled"

pointerControl :: (Applicative m) => Pointer -> Continuation m Interact
pointerControl p = boop (pointerC p)

wheelC :: (Applicative m) => Wheel -> Continuation m (Maybe Button, PosState)
wheelC w = ((generalize _1 (pur $ const (Just ZoomB))) <> (generalize _2 $ pos w))
  where
    pos :: Wheel -> Continuation m PosState
    pos Wheel{deltaUnit, wheelDelta} = pur (diffWheel deltaUnit wheelDelta)

wheelControl :: (Applicative m) => Wheel -> Continuation m (Interact)
wheelControl = boop . wheelC

wheelControl' :: (Applicative m) => Wheel -> Continuation m (Maybe Interact)
wheelControl' = withInitC z0 . wheelControl
  where
    z0 = zoomI . initPosState $ pure 0
    withInitC a = (before (pur onMay)) . maybeC'
      where
        onMay (Just (ZoomI s)) = (Just $ ZoomI s)
        onMay _ = Just a
    
-- $ maybeC' has wrong semantics here.
-- $ We need withInitC to always start this continuation with our initial zoom value 

onTMove :: Throttle m (Pointer -> JSM (Continuation m a)) a
       -> (Pointer -> JSM (Continuation m a))
       -> (T.Text, Prop m a)
onTMove t = runThrottle t onPointerMove

--onPointerUp = listenRaw "pointerup"

unitV :: V3R
unitV = V3 1 1 1


type Eye = V3 R
type UpDir = V3 R


scaleNorm s = (^* s) . normalize

panT :: PosState -> T
panT (_, pd) = f
  where
    f =  Endo $ (over translation (.+^ panDiff))
    panSpeed = 0.1
    upDir = cross eye (pure 0)
    eye = (pure 1)
    scaledChange = pd ^* ((norm eye) * panSpeed)
    panDiff = (scaleNorm (scaledChange ^. _x) (cross eye upDir)) .+^ (scaleNorm (scaledChange ^. _y) upDir)


zoomT :: PosState -> T
zoomT z = Endo $ over translation (^-^ (unitV ^* (zoomFactor . ydiff $ z)))
  where
    zoomFactor x = 1.0 + (x * 0.1)
    ydiff (_, diff) = diff ^. _y



rotateT :: PosState -> T
rotateT (_, delta) = Endo $ \m -> (mkTransformation q' zero) !*! m
  where
    q' = (axisAngle ax' ang)
      where
        ang =  norm md'
    ud = cross ed' (pure 0) --scope.object.up
    ed' = (pure 1) -- (scope.object.position - scope.target)
    ud' = scaleNorm (delta ^. _y) $ ud -- upDir 
    sd' = scaleNorm (delta ^. _y) $ cross ud' ed'
    md' = (V3 (delta ^. _x) (delta ^. _y) 0)
    ax' = cross md' sd'




type InteractM m a = (StateT (Interact) m)

data Interact = ZoomI PosState
              | PanI PosState
              | RotateI PosState
              | Tr Interact Interact
              deriving (Eq, Ord, Show, Generic, NFData, ToJSON)

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
