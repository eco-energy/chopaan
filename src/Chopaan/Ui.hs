{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable
, DeriveTraversable, DeriveAnyClass, GeneralisedNewtypeDeriving, DerivingStrategies, DerivingVia
#-}
module Chopaan.Ui where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Managed

import qualified Streamly.Prelude as S


import qualified Chopaan.Kibbutz.FS as K
import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.NodeId
import Chopaan.Node.HW
import Chopaan.Node.Components

import Data.Maybe

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
    --fonts <- fontSet
    --let s = S.nil
    liftIO $ mainLoop window -- (largeFont fonts)) --s)
  where
    config = defaultWindow
      { windowGraphicsContext = OpenGLContext defaultOpenGL
      , windowHighDPI = True
      , windowResizable = True
      , windowInitialSize = pure 1024
      }



addNode ::
  NodeIdx
  -> NodeMAC
  -> HW Double
  -> (Double, Double)
  -> Maybe NodeIdx
  -> K.NodeModel
addNode i mac hw loc conn = K.NodeModel i mac hw loc (K.Ownership i) (fromMaybe i conn)  

addNodes :: (Monad m) => m ()
addNodes = void $ pure $ do
  addNode (NodeId 1) (NodeId "ac:bcncsad") (HW (SingBC defBC) (SingPC defPC) (SingLC defLC))
      
act :: IO ()
act = do
  withWindowOpen "Hello, Chopaan!" $ do
    --withFont font $ do
      text "Hello, Chopaan!"
      
      button "Click-ity" >>= \case
        False -> return ()
        True -> putStrLn "Ow!"

data FontSet a = FontSet
  { largeFont :: a
  , defaultFont :: a
  } deriving (Functor, Foldable, Traversable)

fontSet :: (MonadIO m) => m (FontSet Font)
fontSet = FontAtlas.rebuild FontSet
  { largeFont = FontAtlas.FromTTF "./fonts/ProggyClean.ttf" 32 Nothing FontAtlas.Latin
  , defaultFont = FontAtlas.DefaultFont 
  } 

mainLoop :: Window -> IO ()
mainLoop window = unlessQuit $ do
  openGL2NewFrame
  sdl2NewFrame
  newFrame
  act
  showDemoWindow
  glClear GL_COLOR_BUFFER_BIT
  render
  openGL2RenderDrawData =<< getDrawData
  glSwapWindow window
  mainLoop window
  where
    unlessQuit action = do
      shouldQuit <- checkEvents
      if shouldQuit then pure () else action
    checkEvents = do
      pollEventWithImGui >>= \case
        Nothing -> return False
        Just event -> (isQuit event ||) <$> checkEvents
    isQuit event = SDL.eventPayload event == SDL.QuitEvent
