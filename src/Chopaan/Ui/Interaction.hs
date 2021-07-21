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
                   , voidC, liftC', leftC', rightC', maybeC', liftCMay', eitherC'
                   , Continuation, pur, impur, kleisli, merge
                   , writeUpdate, shouldUpdate, constUpdate)
import           Shpadoinkle.Console
import           Shpadoinkle.Html (debounce, Debounce(..), throttle, Throttle (..))



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

type PPos = V2 R -- (Double, Double)

newtype Ctrl = Ctrl { unCtrl :: (TVar [Interact]) }
  deriving (Generic)

type PosDiff = (PPos, PPos)

diffPos :: PPos -> Unop PosDiff
diffPos p = \ (e, s) -> (p .-^ e, e)

runCtrl :: (MonadUnliftIO m, Applicative m) => Ctrl -> Continuation m (Maybe Interact) -> m ()
runCtrl (Ctrl t) c = writeUpdate t (listC c)


onStart :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
onStart = onPointerDown (pure . startControl)

onEnd :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
onEnd = onPointerUp (pure . endControl)

move :: (Applicative m) => (T.Text, Prop m (Maybe Interact))
move = onPointerMove (pure . control . pos)

startControl :: (Applicative m) => a -> Continuation m (Maybe Interact)
startControl = const . pur . const . Just . Tr $ identity

control :: (Applicative m) => PPos -> Continuation m (Maybe Interact)
control = maybeC' . control'

endControl :: (Applicative m) => a -> Continuation m (Maybe Interact)
endControl = const . pur . const $ Nothing


control' :: (Applicative m) => PPos -> Continuation m Interact
control' p = (boop' panI (preview #_PanI) $ panC p)
            <> (boop' zoomI (preview #_ZoomI) $ zoomC p)
            <> (boop' rotateI (preview #_RotateI) $ rotateC p)




data PointerType = Touch | Mouse
  deriving (Eq, Ord, Show, Generic, Enum, Bounded, ToJSVal, FromJSVal)

getPointerType :: JSString -> PointerType
getPointerType "touch" = Touch
getPointerType "mouse" = Mouse
getPointerType _ = error "Wrong pointer type"  



data PointerEv = PointerEv
  { pos :: PPos
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
  let b' = Just $ toEnum (b + 1)
  return $ PointerEv (posVec (x, y)) (getPointerType pt) pid b'  


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


unitV :: V3R
unitV = V3 1 1 1

type ZoomS = (V2 R, V2 R)
type PanS = V3 R -- (translate in the x and y planes)

panT :: PanS -> M44R
panT d = over translation (.+^ d) identity

zoomT :: ZoomS -> M44R
zoomT z = over translation (^+^ (unitV ^* (zoomFactor . ydiff $ z))) identity
  where
    zoomFactor x = 1.0 + (x * 0.1)
    ydiff (_, diff) = diff ^. _y

rotateT :: RotateS -> M44R
rotateT = (flip mkTransformation $ zero)

type RotateS = QuatR

type InteractM m a = (StateT (Interact) m)


data Interact = ZoomI ZoomS
              | PanI PanS
              | RotateI RotateS
              | Tr M44R
              deriving (Eq, Ord, Show, Generic, NFData, ToJSON)

boop :: Unop a -> Continuation m a
boop = pur

zoomA :: PPos -> Unop ZoomS
zoomA p (lastP, _) = (p, p ^-^ lastP)

zoomI :: ZoomS -> Interact
zoomI = ZoomI

panI :: PanS -> Interact
panI = PanI

rotateI :: RotateS -> Interact
rotateI = RotateI

panA :: PPos -> Unop PanS
panA = undefined

panC :: PPos -> Continuation m (PanS)
panC x = pur (panA x)

zoomC :: PPos -> Continuation m (ZoomS)
zoomC p = pur (zoomA p) 

rotateA :: PPos -> Unop RotateS
rotateA = undefined

rotateC :: PPos -> Continuation m (RotateS)
rotateC = pur . rotateA

listC :: (Applicative m) => Continuation m (Maybe a) -> Continuation m [a]
listC = liftCMay' (\x a -> a <> (if isJust x then [fromJust x] else [])) (\(a:_) -> pure $ pure a)

evalI :: Interact -> M44R
evalI (ZoomI z) = zoomT z
evalI (PanI p) = panT p
evalI (RotateI r) = rotateT r
evalI (Tr i) = i

data Button = None | RotateB | ZoomB | PanB | TouchRotateB | TouchZoomB
  deriving (Eq, Ord, Show, Generic, Bounded, Enum, ToJSON, NFData)

getI :: Button -> Maybe (PPos -> Interact)
getI None = Nothing
getI RotateB = undefined 

-- interact :: PointEre -> Interact -> Interact 
-- interact (t, loc) i = case t of
--   None -> ChainI $ ParI i UnitI
--   RotateB ->  ChainI $ ParI i (r loc)


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

-- liftZ :: (Functor m) => Continuation m ZoomState -> Continuation m (ZoomState, M44R)
-- liftZ = liftC' (\z (z', t) -> (z, ))) t)) (\(z, t) -> z)
--   where
--     baseScale = (V3 1 1 1)
--     
--      









liftI :: Continuation m (Interact) -> Continuation m M44R
liftI = undefined

act :: Obj -> M44R -> Obj
act = flip transformObj 



posVec :: (R, R) -> PPos
posVec = uncurry V2




type PointEre = (Button, PPos)
path' :: PointerEv -> Unop PointEre
path' = undefined

-- path :: PointerEv -> Continuation (InteractM m a) PointEre
-- path p = impur $ do
--   s <- get
--   let i' = interact p
--   case i' of
--     Nothing -> return ()
--     Just i -> put (s:i)
--   return $ (\(t, pos) -> undefined)
