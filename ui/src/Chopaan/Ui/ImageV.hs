{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ExplicitForAll, TypeOperators, DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, DerivingStrategies, PackageImports, TypeApplications, DataKinds #-}

module Chopaan.Ui.ImageV where

-- import qualified Data.Text as T
-- --import qualified Data.Text.Lazy as LT

-- import GHC.Generics hiding (R)

-- import ConCat.Nat
-- import ConCat.Misc

-- import ConCat.AltCat (toCcc)

-- import ConCat.Circuit (GenBuses, (:>))
-- import ConCat.Graphics.Image
-- import ConCat.Graphics.GLSL
-- import qualified ConCat.Graphics.Color as C
-- import ConCat.Shaped

-- import ConCat.Rebox ()

-- import Control.Monad.IO.Class
-- import Control.Monad (void, when)
-- import Data.Key

-- import Language.Javascript.JSaddle
--        (strToText, valToStr, fromJSVal, toJSVal
--        , JSNull(..), deRefVal, valToObject, js, JSF(..), js1, js4, jsg,
--         valToNumber, (!), (!!), (#), (<#), global, eval, fun, val, array, new, valToText
--        , JSValue(..), call, JSM(..))
-- import GHCJS.DOM (currentWindowUnchecked)
-- import GHCJS.DOM.RequestAnimationFrameCallback (newRequestAnimationFrameCallback)
-- import GHCJS.DOM.Window (Window, requestAnimationFrame)
-- --import GHCJS.DOM.WebGLContext
-- --import "ghcjs-dom" GHCJS.DOM.Document (createElement)
-- --import GHCJS.DOM.Types (Element)

-- import Shpadoinkle
-- import qualified Shpadoinkle.Html as H
-- import Shpadoinkle.Run (runJSorWarp)
-- import Shpadoinkle.Backend.Snabbdom (runSnabbdom)

-- import qualified Chopaan.Ui.Style as Css
-- import Chopaan.Ui.WebGL
-- import Chopaan.View (staticTemplate)

-- --import Language.Javascript.JSaddle
-- --import Control.Lens ((^.))
-- import UnliftIO.Concurrent (forkIO, threadDelay)


-- newtype ShaderEff = ShaderEff { unShaderEff :: T.Text }
--   deriving stock (Eq, Ord, Show, Generic)
--   deriving anyclass (NFData)

-- shaderH :: GenBuses a => Widgets a -> (a :> ImageC) -> ShaderEff
-- shaderH widgets effect = ShaderEff . T.pack . shaderDefs $ glsl widgets effect
-- {-# INLINE shaderH #-} 

-- addShader :: MonadJSM m => ShaderEff -> m ()
-- addShader = H.addScriptSrc . unShaderEff


-- deltaDiskPlot :: forall f.
--   (Foldable f, Functor f, Zip f) =>
--   (R -> C.Color)
--   -> f R
--   -> f R
--   -> ImageC
-- deltaDiskPlot toC = (toC . fst) `C.over` im
--   where
--     im = toImageC $ deltaPlot disk xs ys
--     {-# INLINE im #-}
-- {-# INLINE deltaDiskPlot #-}


-- deltaPlot :: forall f.
--   (Foldable f, Functor f, Zip f)
--   => (R -> R -> R -> Region)
--   -> f R
--   -> f R
--   -> Region
-- deltaPlot toR xs ys = foldl xorR noThing $
--                       (\(x, d) -> translate (x, 0) d)
--                       <$> Data.Key.zip xs (toR <$> ys) 
--   where
--     noThing :: Region
--     noThing = nothing
--     deltas = fmap toR ys
-- {-# INLINE deltaPlot #-}


-- installEffect :: (MonadJSM m) => RawNode -> ShaderEff -> m ()
-- installEffect = undefined


-- -- imListener :: forall m. (MonadJSM m)
-- --   => m ShaderEff
-- --   -> Control
-- --   -> RawNode
-- --   -> Html m ShaderEff
-- -- imListener ma = baked . args
-- --   where
-- --     args :: RawNode -> JSM (RawNode, STM (Continuation m Control))
-- --     args x = pure (x, f)
-- --     f :: STM (Continuation m Control)
-- --     f = pure $ kleisli ma
-- --         ---sss = runShader' unitW (\() -> deltaDiskPlot (\(x, y) -> C.black) undefined)



-- setupShader :: (MonadJSM m) => ShaderEff -> H.Html m ()
-- setupShader (ShaderEff t) = H.canvas ( [H.onLoadM_ $ (liftJSM $ H.addScriptSrc t)]
--                                      <> [H.onClickM_ $ (liftIO $ print t)]
--                                      <> [H.onDragM_ $ (liftJSM $ do
--                                                           w <- currentWindowUnchecked
--                                                           return ()
--                                                           ) ]
--                                      <> [H.class' $
--                                           Css.h_screen <> (Css.bg_red_900)
--                                         ]
--                                      )
--                             []

-- -- shader :: ShaderEff -> Html m ShaderEff
-- -- shader cc = baked $ do
-- --   (notify, stream) <- mkGlobalMailboxAfforded constUpdate
-- --   doc' <- currentDocumentUnchecked
-- --   container' <- toJSVal =<< createElement doc' "canvas"
-- --   --cfg <- mirrorCfg cc
-- --   cm  <- jsg2 "CodeMirror" container' cfg
-- --   _ <- cm ^. js2 "on" "change" (fun $ \_ _ _ -> do
-- --         jsv <- cm ^. js0 "getValue"
-- --         raw :: Maybe Text <- fromJSVal jsv
-- --         maybe (pure ()) (notify . Code--  . encodeUtf8 . TL.fromStrict) raw
-- --       )
-- --   window <- currentWindowUnchecked
-- --   _ <- setTimeout window (fun $ \_ _ _ -> () <$ cm ^. js0 "refresh") (Just 33)
-- --   return (RawNode container', stream)

  

-- animation :: () => Window -> TVar ShaderEff -> (Double -> ShaderEff) -> JSM ()
-- animation w s f = void $ requestAnimationFrame w =<< go where
--   go = newRequestAnimationFrameCallback $ \clock' -> do
--     let clock = clock' - (wait / 1000)
--     liftIO . atomically . (writeTVar s) . f $ clock
--     r <- go
--     when (clock < dur) . void $ requestAnimationFrame w r

-- dur :: Double
-- dur = 3000

-- wait :: Num n => n
-- wait = 3000000

-- view :: (MonadJSM m) => ShaderEff -> Html m ShaderEff
-- view = staticTemplate . setupShader
-- --H.canvas "thing" [text . unShaderEff $ s]


-- {--

-- colorClass :: C.Color -> (T.Text, Prop m ImageC)
-- colorClass c = H.textProperty "style" (LT.toStrict . Css.render . Css.color . toCss $ c)

-- toCss :: C.Color -> Css.Color
-- toCss c = Css.Rgba
--   (round . C.colorR $ c)
--   (round . C.colorB $ c)
--   (round . C.colorG $ c)
--   (realToFrac . C.colorA $ c)

-- imageH :: forall m. Region -> ImageC -> H.Html m ImageC
-- imageH _ _ = H.div "image" []

-- --}
