{-# Language OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Chopaan.Ui.Interaction where

import GHC.Generics
import qualified Data.Text as T
import           GHCJS.DOM.Types              hiding (Text, Touch)
import           Language.Javascript.JSaddle  hiding (JSM, liftJSM, toJSString)

import           Shpadoinkle (listenRaw, Continuation, RawEvent(..))

-- mkOnKey ::  Text -> (KeyCode -> Continuation m a) -> (Text, Prop m a)
-- mkOnKey t f = listenRaw t $ \_ (RawEvent e) ->
--   f <$> liftJSM (fmap round $ valToNumber =<< unsafeGetProp "keyCode" =<< valToObject e)

newtype PointerPos = PointerPos (Double, Double)
  deriving (Eq, Ord, Show, Generic, ToJSVal, FromJSVal)

data PointerType = Touch | Mouse
  deriving (Eq, Ord, Show, Generic, Enum, Bounded, ToJSVal, FromJSVal)

getPointerType :: JSString -> PointerType
getPointerType "touch" = Touch
getPointerType "mouse" = Mouse
getPointerType _ = error "Wrong pointer type"  


data Button = Left | Middle | Right
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

data PointerEv = PointerEv
  { pos :: PointerPos
  , pointerType :: PointerType
  , pointerId :: Int
  , button :: Maybe (Button)
  }

data TrackballActions = Rotate | Zoom | Pan
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)



fromPointer :: RawEvent -> JSM (PointerEv)
fromPointer (RawEvent e') = liftJSM $ do
  e <- valToObject e'
  pt <- valToStr =<< unsafeGetProp "pointerType" e
  x <- valToNumber =<< unsafeGetProp "pageX" e
  y <- valToNumber =<< unsafeGetProp "pageY" e
  pid <- fmap round $ valToNumber =<< unsafeGetProp "pointerId" e
  b <- fmap (round) $ valToNumber =<< unsafeGetProp "button" e
  let b' = if (b > 2 || b < 0) then Nothing else (Just $ toEnum b)
  return $ PointerEv (PointerPos (x, y)) (getPointerType pt) pid b'  



onPointerUp = listenRaw "pointerup"
onPointerDown = listenRaw "pointerdown"

onPointerMove = listenRaw "pointermove"


--onPointerUp = listenRaw "pointerup"
