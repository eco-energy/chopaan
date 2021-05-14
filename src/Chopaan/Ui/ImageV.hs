{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ExplicitForAll, TypeOperators #-}

module Chopaan.Ui.ImageV where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Shpadoinkle
import qualified Shpadoinkle.Html as H

import ConCat.AltCat (toCcc)

import ConCat.Circuit (GenBuses, (:>))
import ConCat.Graphics.Image
import ConCat.Graphics.GLSL
import qualified ConCat.Graphics.Color as C

import ConCat.Rebox ()

import qualified Clay as Css



toCss :: C.Color -> Css.Color
toCss c = Css.Rgba
  (round . C.colorR $ c)
  (round . C.colorB $ c)
  (round . C.colorG $ c)
  (realToFrac . C.colorA $ c)

imageH :: forall m. (R2, R2) -> ImageC -> H.Html m ImageC
imageH ((x, y), (x', y')) im = H.div (colorClass
                                      <$> [im (xt, yt) | xt <- [x..x'], yt <- [y..y']])
                               $ []
  where
    colorClass :: C.Color -> (T.Text, Prop m ImageC)
    colorClass c = H.textProperty "style" (LT.toStrict . Css.render . Css.color . toCss $ c)



effectHtml :: GenBuses a => Widgets a -> (a :> ImageC) -> String
effectHtml widgets effect = unlines $
  [ "<!DOCTYPE html>" , "<html>" , "<head>"
  , "<meta charset='utf-8'/>"
  , "<link rel=stylesheet href=https://code.jquery.com/ui/1.12.1/themes/ui-lightness/jquery-ui.css>"
  , "<script src=https://code.jquery.com/jquery-1.12.4.js></script>"
  , "<script src=https://code.jquery.com/ui/1.12.1/jquery-ui.js></script>"
  , "<script src=script.js></script>"
  , "<link rel=stylesheet href=style.css>"
  , "</head>"
  , "<body onload='go(uniforms,effect)'>"
  , "<div id=ui></div>"
  , "<canvas id=effect></canvas>"
  , "</body>" , "</html>"
  , "<script>"
  , shaderDefs (glsl widgets effect)
  , "</script>" ]


runH :: (GenBuses a)
     => String -> Widgets a -> (a :> ImageC) -> IO ()
-- runH n w f = runHtml n w f
runH n w f = runHtml n w f

runHtml' :: (GenBuses a, C.ToColor c)
         => String -> Widgets a -> (a -> Image c) -> IO ()
runHtml' _ _ _ = error "runHtml' called directly"
{-# NOINLINE runHtml' #-}
{-# RULES "runHtml'"
  forall n w f. runHtml' n w f = runH n w $ toCcc $ toPImageC f #-}
