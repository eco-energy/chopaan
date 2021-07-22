{-# Language OverloadedStrings, DeriveGeneric, DeriveAnyClass, TypeApplications #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes, ImpredicativeTypes, QuantifiedConstraints       #-}
{-# LANGUAGE DataKinds, GADTs                 #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedLabels, NamedFieldPuns     #-}
{-# LANGUAGE FlexibleContexts          #-}


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

import           Shpadoinkle ( listenRaw, Continuation, RawEvent(..), Prop
                             , MonadUnliftIO, JSM, MonadJSM, liftJSM
                             )
import Shpadoinkle (TVar, shpadoinkle
                   , voidC, liftC', leftC', rightC', maybeC', comaybeC', liftCMay', eitherC'
                   , Continuation, pur, impur, kleisli, merge
                   , writeUpdate, shouldUpdate, constUpdate)
import           Shpadoinkle.Console
import           Shpadoinkle.Html (debounce, Debounce(..), throttle, Throttle (..))

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
-- mkOnKey ::  Text -> (KeyCode -> Continuation m a) -> (Text, Prop m a)
-- mkOnKey t f = listenRaw t $ \_ (RawEvent e) ->
--   f <$> liftJSM (fmap round $ valToNumber =<< unsafeGetProp "keyCode" =<< valToObject e)
 -- (Double, Double)

newtype Ctrl = Ctrl { unCtrl :: (TVar [Interact]) }
  deriving (Generic)

type CurPos = Point V2 R
type LastPos = Point V2 R

type PosDiff = Diff (Point V2) R

type PosState = (LastPos, PosDiff)

diffPos :: CurPos -> Unop PosState
diffPos p = \(e, _) -> (p, (p .-. e))



runCtrl :: (MonadUnliftIO m, Applicative m) => Ctrl -> Continuation m (Maybe Interact) -> m ()
runCtrl (Ctrl t) c = writeUpdate t (listC c)


onStart :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
onStart = onPointerDown (pure . startControl)

onEnd :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
onEnd = onPointerUp (pure . endControl)

move :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
move = onPointerMove (pure . controlC . pos)

startControl :: (Applicative m) => a -> Continuation m (Maybe Interact)
startControl = const . pur . const . Just . Tr $ identity

controlC :: (Applicative m) => PosState -> Continuation m (Maybe Interact)
controlC = maybeC' . control'

endControl :: (Applicative m) => a -> Continuation m (Maybe Interact)
endControl = const . pur . const $ Nothing


control' :: (Applicative m) => PosState -> Continuation m Interact
control' p = (boop' panI (preview #_PanI) $ panC p)
            <> (boop' zoomI (preview #_ZoomI) $ zoomC p)
            <> (boop' rotateI (preview #_RotateI) $ rotateC p)




data PointerType = Touch | Mouse
  deriving (Eq, Ord, Show, Generic, Enum, Bounded, ToJSVal, FromJSVal, NFData)

getPointerType :: JSString -> Maybe PointerType
getPointerType "touch" = Just Touch
getPointerType "mouse" = Just Mouse
getPointerType _ = Nothing  



data Pointer = Pointer
  { pos :: CurPos
  , pointerType :: Maybe PointerType
  , pointerId :: Int
  , button :: Maybe (Button)
  }


fromPointer :: RawEvent -> JSM (Maybe Pointer)
fromPointer (RawEvent e') = liftJSM $ do
  e <- valToObject e'
  pt <- valToStr =<< getProp "pointerType" e
  x <- valToNumber =<< getProp "pageX" e
  y <- valToNumber =<< getProp "pageY" e
  pid <- fmap round $ valToNumber =<< getProp "pointerId" e
  b <- fmap (round) $ valToNumber =<< getProp "button" e
  let b' = Just $ toEnum (b + 1)
  debug @ToJSON (b, b')
  return $ Just $ Pointer (zero .+^ posVec (x, y)) (getPointerType pt) pid b'


onEv :: forall m i o. T.Text -> (RawEvent -> (JSM (Maybe i))) -> (i -> JSM (Continuation m o)) -> (T.Text, Prop m o)
onEv ev parse f = listenRaw ev (\_ e ->  (step =<< parse e))
  where
    step (Just x) = f x
    step Nothing = pure . pur $ id

onPointer :: T.Text -> (Pointer -> JSM (Continuation m a)) -> (T.Text, Prop m a)
onPointer act = onEv ("pointer" <> act) fromPointer     

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

type ZoomS = PosState

type Eye = V3 R
type UpDir = V3 R
type PanSpeed = R
type PanS = (R, Eye, PosState, UpDir, V3 R) -- (translate in the x and y planes)

setLength s = (^* s) . normalize

data CState = CState
  { eye :: V3R
  , lastAxis :: V3 R
  , zoomS :: (LastPos, CurPos)
  , panS :: (LastPos, CurPos)
  , pointers :: [PointerType]
  } deriving (Eq, Ord, Show, Generic, NFData)

type T = Endo M44R

panT :: Button -> PanS -> T
panT (panSpeed, eye, (lastP, pd), upDir, pan) = f 
  where
    f =  if ((norm pd) > 0)
               then (over translation (.+^ (pan .+^ panDiff)))
               else id
    panDiff = let
      scaledChange = pd ^* ((norm eye) * panSpeed)
      p' = setLength (pd ^. _x) (cross eye upDir)
      in p'

zoomT :: Button -> ZoomS -> T
zoomT z = over translation (^+^ (unitV ^* (zoomFactor . ydiff $ z)))
  where
    zoomFactor x = 1.0 + (x * 0.1)
    ydiff (_, diff) = diff ^. _y

rotateT :: Button -> RotateS -> T
rotateT r = \m ->  (mkTransformation r zero) !*! m

type RotateS = QuatR

type InteractM m a = (StateT (Interact) m)


data Interact = ZoomI ZoomS
              | PanI PanS
              | RotateI RotateS
              | Tr M44R
              deriving (Eq, Ord, Show, Generic, NFData, ToJSON)

boop :: Unop a -> Continuation m a
boop = pur

zoomA :: Pointer -> Unop ZoomS
zoomA p (lastP, _) = (pos p, pos p .-. lastP)

zoomI :: ZoomS -> Interact
zoomI = ZoomI

panI :: PanS -> Interact
panI = PanI

rotateI :: RotateS -> Interact
rotateI = RotateI

panA :: Pointer -> Unop PanS
panA = undefined

panC :: Pointer -> Continuation m (PanS)
panC x = pur (panA x)

zoomC :: Pointer -> Continuation m (ZoomS)
zoomC p = pur (zoomA p) 

rotateA :: Pointer -> Unop RotateS
rotateA = undefined

rotateC :: Pointer -> Continuation m (RotateS)
rotateC = pur . rotateT

listC :: (Applicative m) => Continuation m (Maybe a) -> Continuation m [a]
listC = liftCMay' (\x a -> a <> (if isJust x then [fromJust x] else [])) (\(a:_) -> pure $ pure a)

evalI :: Interact -> T
evalI (ZoomI z) = zoomT z
evalI (PanI p) = panT p
evalI (RotateI r) = rotateT r
evalI (Tr i) = i

data Button = None | RotateB | ZoomB | PanB | TouchRotateB | TouchZoomB
  deriving (Eq, Ord, Show, Generic, Bounded, Enum, ToJSON, NFData)



boop' :: Applicative m => (a -> Interact) -> (Interact -> Maybe a) -> Continuation m a -> Continuation m Interact
boop' f g = liftCMay' (mergeI . f) g

boopZ :: Applicative m => Continuation m ZoomS -> Continuation m Interact
boopZ = boop' zoomI (preview #_ZoomI)
    
boopP :: Applicative m => Continuation m PanS -> Continuation m Interact
boopP = boop' panI (preview #_PanI)

mergeI :: Interact -> Interact -> Interact
mergeI a b = Tr $ (evalI a) !*! (evalI b)

type TrackballS a = (Interact -> M44R)

trackball :: forall f. (Applicative f, Foldable f, Functor f) => f Interact -> M44R
trackball = undefined


type Unop a = a -> a 


liftI :: Continuation m (Interact) -> Continuation m M44R
liftI = undefined

act :: Obj -> M44R -> Obj
act = flip transformObj 



posVec :: (R, R) -> V2 R
posVec = uncurry V2


type PointEre = (Button, V2 R)

path' :: Pointer -> Unop PointEre
path' = undefined

-- path :: Pointer -> Continuation (InteractM m a) PointEre
-- path p = impur $ do
--   s <- get
--   let i' = interact p
--   case i' of
--     Nothing -> return ()
--     Just i -> put (s:i)
--   return $ (\(t, pos) -> undefined)
