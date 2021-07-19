{-# Language OverloadedStrings, DeriveGeneric, DeriveAnyClass, TypeApplications #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes, ImpredicativeTypes, QuantifiedConstraints       #-}
{-# LANGUAGE DataKinds, GADTs                 #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedLabels, NamedFieldPuns     #-}
{-# LANGUAGE FlexibleContexts          #-}


module Chopaan.Ui.Interaction where

import GHC.Generics hiding (R)
import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State

import qualified Data.Text as T
import           GHCJS.DOM.Types              hiding (Text, Touch)
import           Language.Javascript.JSaddle  hiding (JSM, liftJSM, toJSString)

import           Shpadoinkle (listenRaw, Continuation, RawEvent(..), Prop)
import           Shpadoinkle.Console
import           Shpadoinkle.Html (debounce, Debounce(..), throttle, Throttle (..))

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

type PointerPos = (Double, Double)


data PointerType = Touch | Mouse
  deriving (Eq, Ord, Show, Generic, Enum, Bounded, ToJSVal, FromJSVal)

getPointerType :: JSString -> PointerType
getPointerType "touch" = Touch
getPointerType "mouse" = Mouse
getPointerType _ = error "Wrong pointer type"  


data Button = LeftButton | MiddleScroll | RightButton
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

data PointerEv = PointerEv
  { pos :: PointerPos
  , pointerType :: PointerType
  , pointerId :: Int
  , button :: Maybe (Button)
  }

fromPointer :: RawEvent -> JSM (PointerEv)
fromPointer (RawEvent e') = liftJSM $ do
  e <- valToObject e'
  debug @ToJSVal e 
  pt <- valToStr =<< unsafeGetProp "pointerType" e
  x <- valToNumber =<< unsafeGetProp "pageX" e
  y <- valToNumber =<< unsafeGetProp "pageY" e
  pid <- fmap round $ valToNumber =<< unsafeGetProp "pointerId" e
  b <- fmap (round) $ valToNumber =<< unsafeGetProp "button" e
  let b' = Just MiddleScroll --if (b > 2 || b < 0) then Nothing else (Just $ toEnum b)
  return $ PointerEv (x, y) (getPointerType pt) pid b'  


onPointerUp :: (PointerEv -> JSM (Continuation m a)) -> (T.Text, Prop m a)
onPointerUp f = listenRaw "pointerup" (\_ e -> f =<< fromPointer e)

onPointerDown :: (PointerEv -> JSM (Continuation m a)) -> (T.Text, Prop m a)
onPointerDown f = listenRaw "pointerdown" (\_ e -> f =<< fromPointer e)

onPointerMove :: (PointerEv -> JSM (Continuation m a)) -> (T.Text, Prop m a)
onPointerMove f = listenRaw "pointermove" (\_ e -> f =<< fromPointer e)

onMove :: Throttle m (PointerEv -> JSM (Continuation m a)) a
       -> (PointerEv -> JSM (Continuation m a))
       -> (T.Text, Prop m a)
onMove t = runThrottle t onPointerMove

--onPointerUp = listenRaw "pointerup"


unit :: V3R
unit = V3 1 1 1

type ZoomS = V3 R
type PanS = V2 R -- (translate in the x and y planes)

pan :: R -> PanS -> M44R
pan z d = over translation identity (.+^ (V3 d ^._x d^._y z))

zoom :: ZoomS -> M44R
zoom = over translation identity 

rotate :: RotateS -> M44R
rotate = (fromQuaternion (slerp r r' 0.5))

type RotateS = QuatR

type InteractM m a = (StateT (Interact a) m)


data Interact a where
  Zoom :: Interact ZoomS
  Pan :: Interact PanS
  Rotate :: Interact RotateS

merge :: Interact a -> Interact b -> M44R
merge (Pan z) (Pan p) = (pan z) !*! (pan p) 
merge (Zoom z) (Pan p) = z !*! p
merge (Rotate r) (Rotate r') = mkTransformationMat (fromQuaternion (slerp r r' 0.5)) zero

type TrackballS a = (Interact a -> M44R)

trackball :: forall f. (Applicative f, Foldable f, Functor f) => f (forall a. Interact a) -> M44R
trackball = undefined


type Unop a = a -> a 

-- liftZ :: (Functor m) => Continuation m ZoomState -> Continuation m (ZoomState, M44R)
-- liftZ = liftC' (\z (z', t) -> (z, over translation (^+^ (baseScale ^* (zoomFactor . zdiff $ (z, z')))) t)) (\(z, t) -> z)
--   where
--     baseScale = (V3 1 1 1)
--     zoomFactor x = 1.0 + (x * 0.1)
--      

zoomA :: PointerPos -> Unop ZoomS
zoomA p (lastP, diff) = (posVec p, (posVec p) ^-^ lastP)

panA :: PointerPos -> Unop PanS
panA = undefined

rotateA :: PointerPos -> Unop RotateS
rotateA = undefined

boop :: Unop a -> Continuation m a
boop = pur


ydiff (_, diff) = diff ^. _y

liftI :: Continuation m (Interact a) -> Continuation m M44R
liftI = undefined

act :: Obj -> M44R -> Obj
act = transformObj 

liftR :: Continuation m RotateS -> Continuation m (RotateS, M44R)
liftR = undefined






getAct :: (Applicative m) => (PointerEv -> Continuation m (Interact a, M44R))
getAct = undefined --liftCMay' undefined undefined --toAct mayPan . (liftP . panA)
  where
    pAct :: Obj -> [Interact a] -> Obj
    pAct o is = (Right . Left . fst $ a, snd a !*! snd b)
    mayPan (Right (Left k), a) = Just (k, a)
    mayPan _ = Nothing
    zAct :: (ZoomS, M44R) -> (Interact a, M44R) -> (Interact a, M44R)
    zAct a b = (Left . fst $ a, snd a !*! snd b)
    mayZoom (Left k, a) = Just (k, a)
    mayZoom _ = Nothing
    rAct :: (RotateS, M44R) -> (Interact a, M44R) -> (Interact a, M44R)
    rAct a b = (Right . Right . fst $ a, snd b !*! snd a)
    mayRot (Right (Right k), a) = Just (k, a)
    mayRot _ = Nothing


defActS = (Just Zoom, (Left zero, identity))

getActionType :: Button -> Interact a
getActionType = const Zoom

posVec :: PointerPos -> V2 R
posVec (x , y) = V2 x y

zoomVec :: PointerPos -> Interact a
zoomVec p = Left $ (posVec p, posVec p)

panVec :: PointerPos -> Interact a
panVec = Right . Left . posVec

rotVec :: PointerPos -> Interact a
rotVec = Right . Right . posVec

interact :: PointEre -> Maybe (Interact a)
interact (t, loc) = case (toEnum (t + 1)) of
  None -> Nothing
  RotateB -> Rotate (defQuat)
  

type PointEre = (PoinType, V2 R)

data PoinType = None | RotateB | ZoomB | PanB | TouchRotateB | TouchZoomB
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

instance Semigroup PoinType where
  None <> None = Node
  None <> a = a
  a <> None = a

path' :: PointerEv -> Unop PointEre
path' = undefined

path :: PointerEv -> Continuation (InteractM m a) PointEre
path p = impur $ do
  s <- get
  let i' = interact p
  case i' of
    Nothing -> return ()
    Just i -> put (s:i)
  return $ (\(t, pos) -> )
  

startAction :: PointerEv -> Continuation m (Maybe (Interact a)) 
startAction PointerEv{button, pos} = pur $
  (\_ -> Just (getActionType <$> button, (zoomVec pos, identity)))

endAction :: (MonadJSM m) => PointerEv -> Continuation m (Maybe (Interact b))
endAction PointerEv{button, pos} = pur $
  fmap (\_ -> (Nothing, (zoomVec pos, identity)))
                      
controlTf :: Applicative m => PointerEv -> Continuation m (TrackballS a)
controlTf = rightC' . getAct
