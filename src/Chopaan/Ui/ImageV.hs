{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ExplicitForAll, TypeOperators, DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, DerivingStrategies, PackageImports, TypeApplications, DataKinds #-}

module Chopaan.Ui.ImageV where

import qualified Data.Text as T
--import qualified Data.Text.Lazy as LT

import GHC.Generics hiding (R)

import ConCat.Nat
import ConCat.Misc

import ConCat.AltCat (toCcc)

import ConCat.Circuit (GenBuses, (:>))
import ConCat.Graphics.Image
import ConCat.Graphics.GLSL
import qualified ConCat.Graphics.Color as C
import ConCat.Shaped

import ConCat.Rebox ()

import Control.Monad.IO.Class
import Control.Monad (void, when)
import Data.Key

import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.RequestAnimationFrameCallback (newRequestAnimationFrameCallback)
import GHCJS.DOM.Window (Window, requestAnimationFrame)
--import "ghcjs-dom" GHCJS.DOM.Document (createElement)
--import GHCJS.DOM.Types (Element)

import Shpadoinkle
import qualified Shpadoinkle.Html as H
import Shpadoinkle.Run (runJSorWarp)
import Shpadoinkle.Backend.ParDiff (runParDiff, stage)
--import qualified Clay as Css

--import Language.Javascript.JSaddle
--import Control.Lens ((^.))
import UnliftIO.Concurrent (forkIO, threadDelay)


newtype ShaderEff = ShaderEff { unShaderEff :: T.Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

shaderH :: GenBuses a => Widgets a -> (a :> ImageC) -> ShaderEff
shaderH widgets effect = ShaderEff . T.pack . shaderDefs $ glsl widgets effect

addShader :: MonadJSM m => ShaderEff -> m ()
addShader = H.addScriptSrc . unShaderEff


deltaDiskPlot :: forall f.
  (Foldable f, Functor f, Zip f) =>
  (R -> C.Color)
  -> f R
  -> f R
  -> ImageC
deltaDiskPlot toC xs ys = (toC . fst) `C.over` im
  where
    im = toImageC $ deltaPlot disk xs ys

deltaPlot :: forall f.
  (Foldable f, Functor f, Zip f)
  => (R -> Region)
  -> f R
  -> f R
  -> Region
deltaPlot toR xs ys = foldl xorR noThing $
                      (\(x, d) -> translate (x, 0) d)
                      <$> Data.Key.zip xs deltas 
  where
    noThing :: Region
    noThing = nothing
    deltas = fmap toR ys


getCanvas :: MonadJSM m => m (RawNode)
getCanvas = H.getById "effect"

installEffect :: (MonadJSM m) => RawNode -> ShaderEff -> m ()
installEffect = undefined


imListener :: forall m. (MonadJSM m)
  => (ShaderEff -> m (Continuation m ShaderEff))
  -> RawNode
  -> Html m ShaderEff
imListener ma = baked . args
  where
    args :: RawNode -> JSM (RawNode, STM (Continuation m ShaderEff))
    args x = pure (x, f)
    f :: STM (Continuation m ShaderEff)
    f = pure $ Continuation  (id, ma)
        ---sss = runShader' unitW (\() -> deltaDiskPlot (\(x, y) -> C.black) undefined)


animation :: () => Window -> TVar ShaderEff -> (Double -> ShaderEff) -> JSM ()
animation w s f = void $ requestAnimationFrame w =<< go where
  go = newRequestAnimationFrameCallback $ \clock' -> do
    let clock = clock' - (wait / 1000)
    liftIO . atomically . (writeTVar s) . f $ clock
    r <- go
    when (clock < dur) . void $ requestAnimationFrame w r

dur :: Double
dur = 3000

wait :: Num n => n
wait = 3000000

view :: ShaderEff -> Html m ShaderEff
view s = H.canvas "thing" [text . T.pack . show $ s]


{--

colorClass :: C.Color -> (T.Text, Prop m ImageC)
colorClass c = H.textProperty "style" (LT.toStrict . Css.render . Css.color . toCss $ c)

toCss :: C.Color -> Css.Color
toCss c = Css.Rgba
  (round . C.colorR $ c)
  (round . C.colorB $ c)
  (round . C.colorG $ c)
  (realToFrac . C.colorA $ c)

imageH :: forall m. Region -> ImageC -> H.Html m ImageC
imageH _ _ = H.div "image" []

--}
