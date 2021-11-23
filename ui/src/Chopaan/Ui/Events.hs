{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# Language OverloadedStrings, DeriveGeneric, DeriveAnyClass, PackageImports #-}
module Chopaan.Ui.Events where

import GHC.Generics hiding (R)

import Control.Monad

import Data.Aeson
import Data.Text as T

import GHCJS.DOM (currentWindowUnchecked, currentDocumentUnchecked)
import "ghcjs-dom" GHCJS.DOM.Document (getDocumentElementUnchecked)
import GHCJS.DOM.Element (getBoundingClientRect, getClientTop, getClientLeft, Element)
import GHCJS.DOM.Window (getInnerHeight, getInnerWidth, getPageXOffset, getPageYOffset)
import GHCJS.DOM.DOMRectReadOnly (getTop, getLeft) --getWidth, getHeight, getLeft)

import Shpadoinkle
import Shpadoinkle.Html (preventDefault)
import Shpadoinkle.Console (debug)

import Linear.V2

import           Language.Javascript.JSaddle  hiding (JSM, MonadJSM, liftJSM, toJSString)

import Chopaan.Ui.Base


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


data PointerType = Touch | Mouse
  deriving (Eq, Ord, Show, Generic, Enum, Bounded, ToJSON, NFData)

getPointerType :: JSString -> Maybe PointerType
getPointerType "touch" = Just Touch
getPointerType "mouse" = Just Mouse
getPointerType _ = Nothing


data Button = NoneB | RotateB | ZoomB | PanB | TouchRotateB | TouchZoomB
  deriving (Eq, Ord, Show, Generic, Bounded, Enum, ToJSON, NFData)


data Pointer = Pointer
  { pos :: CurPos
  , pointerType :: Maybe PointerType
  , pointerId :: Int
  , button :: Button
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON)


toPointer :: Screen -> RawEvent -> JSM (Maybe Pointer)
toPointer screen (RawEvent e') = liftJSM $ do
  preventDefault (RawEvent e')
  e <- valToObject e'
  pt <- valToStr =<< getProp "pointerType" e
  x <- valToNumber =<< getProp "pageX" e
  y <- valToNumber =<< getProp "pageY" e
  pid <- fmap round $ valToNumber =<< getProp "pointerId" e
  b <- fmap (round) $ valToNumber =<< getProp "button" e
  let b' = toEnum (b + 1)
  ps <- case b' of
    ZoomB -> posOnScreen screen x y 
    PanB -> posOnScreen screen x y
    RotateB -> posOnCircle screen x y
    _ -> posOnScreen screen x y
  debug @ToJSON b'
  return $ Just $ Pointer ps (getPointerType pt) pid b'


posOnCircle :: Screen -> R -> R -> JSM CurPos
posOnCircle s x' y' = return $ toPos (x, y)
  where
    x = (x' - (widthG s) * 0.5 - (left s)) / (widthG s * 0.5)
    y = ((heightG s) + 2 * (top s - y')) / (widthG s)

posOnScreen :: Screen -> R -> R -> JSM CurPos
posOnScreen s x' y' = return $ toPos (x, y)
  where
    x = (x' - (left s)) / (widthG s)
    y = (y' - top s) / (heightG s)


onEv :: forall m i o. T.Text -> (RawEvent -> (JSM (Maybe i))) -> (i -> JSM (Continuation m o)) -> (T.Text, Prop m o)
onEv ev parse f = listenRaw ev (\_ e ->  (step =<< parse e))
  where
    step (Just x) = f x
    step Nothing = pure . pur $ id

onPointer :: Screen -> T.Text -> (Pointer -> JSM (Continuation m a)) -> (T.Text, Prop m a)
onPointer s act = onEv ("pointer" <> act) (toPointer s)     

onPointerUp :: Screen -> (Pointer -> JSM (Continuation m a)) -> (T.Text, Prop m a)
onPointerUp s = onPointer s "up"
onPointerDown :: Screen -> (Pointer -> JSM (Continuation m a)) -> (T.Text, Prop m a)
onPointerDown s = onPointer s "down"
onPointerMove :: Screen -> (Pointer -> JSM (Continuation m a)) -> (T.Text, Prop m a)
onPointerMove s = onPointer s "move"


noRightClick :: (T.Text, Prop m ())
noRightClick = listenRaw "contextmenu" $ (\_ e -> do
                                             (preventDefault e)
                                             return $ pur (const ())
                                         )

getWH :: MonadJSM m => m (Double, Double)
getWH = do
  w <- currentWindowUnchecked
  height <- realToFrac <$> getInnerHeight w
  width <- realToFrac <$> getInnerWidth w
  return (width, height)


data Screen = Screen
  { widthG :: Double
  , heightG :: Double
  , left :: Double
  , top :: Double
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

zeroScreen :: Screen
zeroScreen = Screen 0 0 0 0

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
