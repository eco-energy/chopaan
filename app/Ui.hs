{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ExplicitForAll, TypeApplications, TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# OPTIONS_GHC -dsuppress-idinfo #-}
{-# OPTIONS_GHC -dsuppress-uniques #-}
{-# OPTIONS_GHC -dsuppress-module-prefixes #-}

-- {-# OPTIONS_GHC -ddump-simpl #-}

-- {-# OPTIONS_GHC -ddump-rule-rewrites #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=25 #-}  -- default 100
-- {-# OPTIONS_GHC -fsimpl-tick-factor=250 #-}  -- default 100

-- {-# OPTIONS -fplugin-opt=ConCat.Plugin:trace #-}

{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}

module Main where

import Prelude hiding (id, const, (.), id)
import ConCat.Circuit
import ConCat.Graphics.Image
import qualified ConCat.Graphics.Color as C
import ConCat.Graphics.GLSL
import ConCat.Shaped
import ConCat.Nat
import ConCat.Misc

import ConCat.AltCat
import ConCat.Rebox ()

import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.RequestAnimationFrameCallback (newRequestAnimationFrameCallback)
import GHCJS.DOM.Window (Window, requestAnimationFrame)

import Shpadoinkle
import qualified Shpadoinkle.Html as H
import Shpadoinkle.Backend.ParDiff (runParDiff, stage)
import Shpadoinkle.Run (runJSorWarp, live)

import UnliftIO.Concurrent (forkIO, threadDelay)
import Chopaan.Ui.ImageV


main :: IO ()
main = appx

{--
We want a canvas element that can be passed to install_effect from
and we need a go function which installs that effect.
We also need debounced event handlers for dragging and scrolling.

--}

app :: IO ()
app = live 8080 $ do
  --let canvas = H.canvas [("delta-thingo", ""), ("w", deltaWidget )] []
  return ()


unitS :: ShaderEff
unitS = runShader' "unitS" (pairW (sliderW "Outer" (0,2) 1) timeW) $
      \ (o,i) -> annulus o ((sin i + 1) / 2)
      --unitW (const $ deltaDiskPlot (const C.black) x y)
  --where
    --x :: _ --Maybe (Maybe (Maybe (Maybe (Maybe Double))))
    --x = pure 5
    --y :: Maybe (Maybe (Maybe (Maybe (Maybe Double))))
    --y = pure 5

appx :: IO ()
appx = do
  t <- newTVarIO unitS
  runJSorWarp 8080 $ do
    w <- currentWindowUnchecked
    _ <- forkIO $ threadDelay wait >> animation w t f
    shpadoinkle id runParDiff t view stage
  where
    f :: Double -> ShaderEff
    f = const unitS


runS :: (GenBuses a) => Widgets a -> (a :> ImageC) -> ShaderEff
runS w c = shaderH w c

runShader' :: (GenBuses a, C.ToColor c)
        => String
        -> Widgets a
        -> (a -> Image c)
        -> ShaderEff
runShader' _  _ _ = error "runShader' called directly"
{-# NOINLINE runShader' #-}
{-# RULES "runShader'"
  forall n w f. runShader' n w f = runS w $ toCcc $ toPImageC f #-}
