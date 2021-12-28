{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable
, DeriveTraversable, DeriveAnyClass, GeneralisedNewtypeDeriving, DerivingStrategies, DerivingVia
#-}
module Chopaan.Ui where

import qualified Streamly.Prelude as S
import qualified Chopaan.Kibbutz.FS as K
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Managed

import DearImGui
import DearImGui.OpenGL2
import qualified DearImGui.FontAtlas as FontAtlas
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.GL
import SDL

main :: IO ()
main = do
  initializeAll
  runManaged $ do
    window <- do
      let title = "Hello, Chopaan!"
      managed $ bracket (createWindow title config) destroyWindow
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext
    _ <- managed $ bracket createContext destroyContext
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown
    fonts <- fontSet
    let s = S.nil
    liftIO $ mainLoop window (act (largeFont fonts) s)
  where
    config = defaultWindow
      { windowGraphicsContext = OpenGLContext defaultOpenGL
      , windowHighDPI = True
      , windowResizable = True
      , windowInitialSize = pure 1024
      }
      
act :: Font -> S.SerialT IO K.Kbtzim -> IO ()
act font ts = withWindowOpen "Hello, Chopaan!" $ do
  withFont font $ do
    text "Hello, Chopaan!"
    button "Click-ity" >>= \case
      False -> return ()
      True -> putStrLn "Ow!"
    --showDemoWindow

data FontSet a = FontSet
  { largeFont :: a
  , defaultFont :: a
  } deriving (Functor, Foldable, Traversable)

fontSet :: (MonadIO m) => m (FontSet Font)
fontSet = FontAtlas.rebuild FontSet
  { largeFont = FontAtlas.FromTTF "./fonts/ProggyClean.ttf" 32 Nothing FontAtlas.Latin
  , defaultFont = FontAtlas.DefaultFont 
  } 

mainLoop :: Window -> IO () -> IO ()
mainLoop window frameAction = loop
  where
    loop = unlessQuit $ do
      openGL2NewFrame
      sdl2NewFrame
      newFrame
      frameAction
      glClear GL_COLOR_BUFFER_BIT
      render
      openGL2RenderDrawData =<< getDrawData
      glSwapWindow window
      loop
    unlessQuit action = do
      shouldQuit <- checkEvents
      if shouldQuit then pure () else action
    checkEvents = do
      pollEventWithImGui >>= \case
        Nothing -> return False
        Just event -> (isQuit event ||) <$> checkEvents
    isQuit event = SDL.eventPayload event == SDL.QuitEvent
