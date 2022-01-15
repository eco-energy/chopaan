{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable
, DeriveTraversable, DeriveAnyClass, GeneralisedNewtypeDeriving, DerivingStrategies, DerivingVia
#-}
{-# LANGUAGE TypeOperators, TypeApplications, ScopedTypeVariables, ConstraintKinds, FlexibleContexts #-}
module Chopaan.Ui where


import GHC.Generics
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Managed

import ConCat.Misc ((:*),R,sqr,magSqr,Unop,Binop,inNew,inNew2)
import ConCat.Circuit (GenBuses,(:>))
import ConCat.Graphics.GLSL
import ConCat.Graphics.Color
import ConCat.Graphics.Image
import qualified ConCat.RunCircuit as RC
import ConCat.Syntactic (Syn,render)
import ConCat.AltCat (Ok2,toCcc,(:**:)(..))
import qualified ConCat.AltCat as A

import ConCat.Rebox () -- necessary for reboxing rules to fire

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
import Chopaan.Kibbutz.Ui


type MonConstraint t m n a = ( S.IsStream t, S.MonadAsync m, K.HasPath n
                           , Renderable n, Renderable a, MonadRender m )

class Renderable a where
  renderI :: a -> (ImageC, Region)

instance (Renderable a, Renderable b) => Renderable (a, b) where
  renderI (a, b) = ((A.liftA2' overC colA colB), unionR regA regB)
    where
      (colA, regA) = renderI a
      (colB, regB) = renderI b
  
  
class (Monad m) => MonadRender m where
  renderM :: ImageC -> Region -> m ()
  
newtype RenderM m a = RenderM (m a)


monitor :: forall t m n a. (MonConstraint t m n a) => t m (n, a) -> m ()
monitor = S.foldlM' (\_ a -> (uncurry renderM) . renderI $ a) (pure ()) . S.adapt 


type GO a b = (GenBuses a, Ok2 (:>) a b)

runSyn :: Syn a b -> IO ()
runSyn syn = putStrLn ('\n' : ConCat.Syntactic.render syn)

runCirc :: GO a b => String -> (a :> b) -> IO ()
runCirc nm circ = RC.run nm [] circ


-- glsl' :: (GenBuses a, ToColor c)
--          => String -> Widgets a -> (a -> Image c) -> Shader a
-- glsl' _ _ _ = error "glsl' called directly"
-- {-# NOINLINE glsl' #-}
-- {-# RULES "glsl'"
--   forall n w f. glsl' n w f = runH n w $ toCcc $ toPImageC f #-}


main :: IO ()
main = do
  initializeAll
  runManaged $ do
    window <- do
      managed $ bracket (createWindow title config) destroyWindow
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext
    _ <- managed $ bracket createContext destroyContext
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown
    --fonts <- fontSet
    --let s = S.nil
    liftIO $ mainLoop window -- (largeFont fonts)) --s)
  where
    title = "Chopaan"
    config = defaultWindow
      { windowGraphicsContext = OpenGLContext defaultOpenGL
      , windowHighDPI = True
      , windowResizable = True
      , windowInitialSize = pure 1024
      }

      
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
  DearImGui.render
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
