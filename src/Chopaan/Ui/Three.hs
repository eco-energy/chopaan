{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes, TypeFamilies #-}
module Chopaan.Ui.Three where

import GHC.Generics
import Control.Newtype.Generics
import ConCat.Misc

import Diagrams.Prelude hiding (unit, (#), Camera)

import Control.Monad (void)
import Data.Text hiding (pack)
import GHCJS.DOM
import Language.Javascript.JSaddle
import Text.RawString.QQ

unit :: Num a => V3 a
unit = mkR3 1 1 1

origin :: Num a => P3 a
origin = mkP3 0 0 0

one :: Fractional a => V3 a
one = project unit unit

toSpherical :: forall a. (RealFloat a) => V3 a -> (a, Angle a, Angle a)
toSpherical = view r3SphericalIso


newtype Renderer = Renderer { unRenderer :: JSVal }
  deriving (ToJSVal, Generic)
instance Newtype Renderer

newtype Scene = Scene { unScene :: JSVal }
  deriving (ToJSVal, Generic)
instance Newtype Scene

newtype Camera = Camera { unCamera :: JSVal }
  deriving (ToJSVal, Generic)
instance Newtype Camera




createRendererJS :: Text
createRendererJS = [r|createRenderer = function () {} |]

createSceneJS :: Text
createSceneJS = [r|createScene = function () {} |]

createCameraJS = [r|createScene = function () {} |]
  
mkRenderer :: MonadJSM m => Text -> m Renderer
mkRenderer args = onWindow createRendererJS ("createRenderer" :: Text) args

mkScene :: MonadJSM m => Text -> m Scene
mkScene args = onWindow createSceneJS ("createScene" :: Text) args

mkCamera :: MonadJSM m => Text -> m Camera
mkCamera args = onWindow createCameraJS ("createCamera" :: Text) args



onWindow :: MonadJSM m
  => (ToJSVal n, Newtype o, O o ~ JSVal)
  => Text
  -> Text
  -> n
  -> m o
onWindow jsInitSrc fnName args = liftJSM $ do
  _ <- eval jsInitSrc
  w <- toJSVal =<< currentWindowUnchecked
  u <- toJSVal args
  pack <$> (w # fnName $ [u])
