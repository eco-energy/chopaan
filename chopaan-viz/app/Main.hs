{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Chopaan Grid Playground — a Dear ImGui toy for poking at power-flow.
--
-- The renderee is an /algebraic-graphs/ @Graph Int@ (a 4-node mgenv feeder).
-- Every frame it is packed into the categorified Hopfield kernel
-- (@Kernel.settle@, an FFI call into @cbits/hopfield_step.c@ — the very C that
-- Categorifier lowered chopaan's Hopfield step to) and the settled edge flows
-- are drawn. Drag nodes, rewire edges (algebraic overlay/connect), slide the PV
-- setpoint / damping / conductances / time-of-day, and watch the grid-import
-- "cost" score respond. Cycle through the real mgenv feeders with Next.
--
-- Build/run: see README.md (needs SDL2 + OpenGL + the dear-imgui package).
-- Draw-list / widget calls target dear-imgui ~2.2; tweak if your version differs.
module Main (main) where

import           Control.Monad (unless, when, forM_, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Managed (managed, managed_, runManaged)
import           Control.Exception (bracket, bracket_)
import           Data.Bits ((.|.), shiftL)
import           Data.IORef
import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Word (Word32)
import           Text.Printf (printf)

import qualified Algebra.Graph as G

import qualified SDL

import           DearImGui
import           DearImGui.OpenGL3
import           DearImGui.SDL
import           DearImGui.SDL.OpenGL
import           DearImGui.Raw (ImVec2(..), ImU32)
import qualified DearImGui.Raw.DrawList as DL
import           Foreign.C.Types (CFloat, CInt)

import           Kernel (Flows(..), settle)
import           Feeder (Feeder(..), loadFeeders, demoFeeder)

--------------------------------------------------------------------------------
-- App state (individual IORefs so the ImGui widgets bind directly)

data App = App
  { appFeeders :: [Feeder]
  , appCur     :: IORef Int
  , appGraph   :: IORef (G.Graph Int)          -- topology (≤ 4 nodes, ≤ 3 edges)
  , appPos     :: IORef (Map Int (Float,Float)) -- node screen positions
  , appCond    :: IORef (Map (Int,Int) Float)  -- conductance keyed by edge (min,max)
  , appPSet    :: IORef Float                  -- PV setpoint at node 1, 0..1 of rating
  , appAlpha   :: IORef Float                  -- Hopfield damping
  , appHour    :: IORef Float                  -- time of day 0..23
  , appXflow   :: IORef [Double]               -- 6 persisted edge flows (P,Q ×3)
  , appFlows   :: IORef Flows
  , appGrab    :: IORef (Maybe Int)
  , appMouse   :: IORef (Float,Float)
  , appDown    :: IORef Bool
  }

-- Node roles are fixed by index for the 4-node kernel: 0 slack, 1 PV, 2/3 load.
data Role = Slack | PVBus | LoadBus deriving Eq
roleOf :: Int -> Role
roleOf 0 = Slack
roleOf 1 = PVBus
roleOf _ = LoadBus

ukey :: Int -> Int -> (Int,Int)
ukey a b = (min a b, max a b)

--------------------------------------------------------------------------------
-- Physics-facing helpers (mirror cbits/hopfield_step.c's env driver)

solar :: Float -> Double -> Double
solar h rated
  | h < 6 || h > 18 = 0
  | otherwise       = let x = (realToFrac h - 12)/6 in max 0 (rated * (1 - x*x))

loadCurve :: Float -> Double -> Double
loadCurve h base =
  let dm = realToFrac h - 8; de = realToFrac h - 19
      m  = 0.6 + 0.4*exp(-dm*dm*0.25); e = 0.7 + 0.5*exp(-de*de/3.0)
  in base * max m e

tariff :: Float -> Double
tariff h = if h >= 18 && h <= 22 then 30 else 15

-- | Assemble the 30-double kernel input from the current graph + sliders.
packInput :: App -> Feeder -> IO [Double]
packInput app fd = do
  g     <- readIORef (appGraph app)
  cond  <- readIORef (appCond app)
  pset  <- readIORef (appPSet app)
  h     <- readIORef (appHour app)
  a     <- readIORef (appAlpha app)
  xf    <- readIORef (appXflow app)
  let es      = take 3 (G.edgeList g)                 -- edge slots 0..2
      bAt n s = case drop s es of
                  ((u,v):_) | n==u ->  1 | n==v -> -1
                  _                -> 0
      incidence = [ fromIntegral (bAt n s) | n <- [0..3], s <- [0..2] ]   -- 12
      ld n      = loadCurve h (fdLoad fd !! n)
      pRate     = fdRated fd !! 1
      pSet      = min (solar h pRate) (realToFrac pset * pRate)
      injP      = [ 0
                  , pSet - ld 1
                  , negate (ld 2)
                  , negate (ld 3) ]
      injQ      = map (*0.3) injP
      -- conductance in edge-slot order (edgeList order), keyed by the edge
      gs        = [ realToFrac (Map.findWithDefault 1 (ukey u v) cond)
                  | (u,v) <- es ] ++ replicate (3 - length es) 1          -- 3
  pure $ incidence ++ injP ++ injQ ++ gs ++ xf ++ [realToFrac a]

--------------------------------------------------------------------------------
-- Colours (ImU32 packs as 0xAABBGGRR)

-- NB: dear-imgui's ImU32 is a Word32 (0xAABBGGRR). If your version wraps it in
-- a newtype, replace `fromIntegral w` with its constructor.
col :: Int -> Int -> Int -> Int -> ImU32
col r g b a =
  let w = (a `shiftL` 24) .|. (b `shiftL` 16) .|. (g `shiftL` 8) .|. r :: Int
  in fromIntegral (fromIntegral w :: Word32)

roleColour :: Role -> ImU32
roleColour Slack   = col 90 160 250 255   -- blue  grid-tie
roleColour PVBus   = col 250 205 70  255   -- amber PV
roleColour LoadBus = col 170 170 180 255   -- grey  load

-- edge colour by real-power flow: green outward, red inward, brightness by |P|
edgeColour :: Double -> ImU32
edgeColour p =
  let m = min 1 (abs p / 40)
      i = round (60 + 195*m) :: Int
  in if p >= 0 then col 60 i 90 255 else col i 70 70 255

--------------------------------------------------------------------------------
-- Rendering

v2 :: Float -> Float -> ImVec2
v2 = ImVec2

drawGraph :: App -> IO ()
drawGraph app = do
  dl    <- DL.getForegroundDrawList
  g     <- readIORef (appGraph app)
  pos   <- readIORef (appPos app)
  fl    <- readIORef (appFlows app)
  let es    = take 3 (G.edgeList g)
      p n   = fromMaybe (100,100) (Map.lookup n pos)
  -- edges (behind nodes)
  forM_ (zip [0..] es) $ \(i,(u,v)) -> do
    let (ux,uy) = p u; (vx,vy) = p v
        pw      = maybe 0 fst (lookup i (zip [0..] (fEdge fl)))
        thick   = realToFrac (2 + min 8 (abs pw/6)) :: CFloat
    DL.addLine dl (v2 ux uy) (v2 vx vy) (edgeColour pw) thick
  -- nodes
  forM_ (G.vertexList g) $ \n -> do
    let (x,y) = p n
    DL.addCircleFilled dl (v2 x y) (18::CFloat) (roleColour (roleOf n)) (24::CInt)
    DL.addCircle       dl (v2 x y) (18::CFloat) (col 20 20 25 255) (24::CInt) (2::CFloat)
    -- label (node id); if your dear-imgui lacks DrawList.addText, drop this line
    DL.addText dl (v2 (x-4) (y-7)) (col 20 20 25 255) (T.pack (show n))

--------------------------------------------------------------------------------
-- Interaction: drag nodes with the mouse when ImGui isn't using it

handleDrag :: App -> IO ()
handleDrag app = do
  captured <- wantCaptureMouse
  down     <- readIORef (appDown app)
  (mx,my)  <- readIORef (appMouse app)
  pos      <- readIORef (appPos app)
  grab     <- readIORef (appGrab app)
  if not down || captured
    then writeIORef (appGrab app) Nothing
    else case grab of
      Just n  -> modifyIORef' (appPos app) (Map.insert n (mx,my))
      Nothing -> do
        let near = sortOn (\(_, (x,y)) -> (x-mx)^(2::Int) + (y-my)^(2::Int)) (Map.toList pos)
        case near of
          ((n,(x,y)):_) | (x-mx)^(2::Int)+(y-my)^(2::Int) < 26*26 ->
              writeIORef (appGrab app) (Just n)
          _ -> pure ()

--------------------------------------------------------------------------------
-- Controls panel

controls :: App -> IO ()
controls app = withWindowOpen "Controls" $ do
  fl  <- liftIO (readIORef (appFlows app))
  h   <- liftIO (readIORef (appHour app))
  cur <- liftIO (readIORef (appCur app))
  let imp  = fImport fl
      cost = (if imp > 0 then imp else 0.5*imp) * tariff h
  text (T.pack (printf "mgenv feeder #%d of %d" cur (length (appFeeders app))))
  text (T.pack (printf "grid import : %+8.3f kW" imp))
  text (T.pack (printf "cost score  : %+8.2f PKR/h  (lower is better)" cost))
  separator
  void $ sliderFloat "PV setpoint" (appPSet app) 0 1
  void $ sliderFloat "damping a"   (appAlpha app) 0.05 0.95
  void $ sliderFloat "hour"        (appHour app) 0 23
  g     <- liftIO (readIORef (appGraph app))
  conds <- liftIO (readIORef (appCond app))
  forM_ (take 3 (G.edgeList g)) $ \(u,v) -> do
    r <- liftIO (newIORef (Map.findWithDefault 1 (ukey u v) conds))
    changed <- sliderFloat (T.pack (printf "g %d-%d" u v)) r 0.01 1.0
    when changed $ liftIO $ do
      val <- readIORef r
      modifyIORef' (appCond app) (Map.insert (ukey u v) val)
  separator
  nextClicked <- button "Next feeder"
  sameLine
  resetClicked <- button "Reset flows"
  when nextClicked  $ liftIO (cycleFeeder app)
  when resetClicked $ liftIO (writeIORef (appXflow app) (replicate 6 0))
  text "drag nodes with the mouse • amber=PV blue=slack grey=load"
  text "green edge = power flowing out, red = flowing in"

--------------------------------------------------------------------------------
-- Feeder loading / cycling

feederToState :: App -> Feeder -> IO ()
feederToState app fd = do
  writeIORef (appGraph app) (edgesToGraph (fdEdges fd))
  writeIORef (appCond  app) (Map.fromList
    [ (ukey u v, realToFrac c) | ((u,v),c) <- zip (fdEdges fd) (fdCond fd) ])
  writeIORef (appXflow app) (replicate 6 0)
  -- circular default layout centred on screen
  let cx=520; cy=360; r=210 :: Float
      place k = ( cx + r*cos(2*pi*fromIntegral k/4)
                , cy + r*sin(2*pi*fromIntegral k/4) )
  writeIORef (appPos app) (Map.fromList [ (k, place k) | k <- [0..3] ])

edgesToGraph :: [(Int,Int)] -> G.Graph Int
edgesToGraph es = G.overlay (G.vertices [0..3]) (G.edges (take 3 es))

cycleFeeder :: App -> IO ()
cycleFeeder app = do
  i <- readIORef (appCur app)
  let n  = length (appFeeders app)
      i' = if n == 0 then 0 else (i+1) `mod` n
  writeIORef (appCur app) i'
  feederToState app (appFeeders app !! i')

--------------------------------------------------------------------------------
-- Main loop

main :: IO ()
main = do
  feeders0 <- loadFeeders "grids_4node.json"
  let feeders = if null feeders0 then [demoFeeder] else feeders0
  SDL.initializeAll
  runManaged $ do
    let cfg = SDL.defaultWindow
                { SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
                , SDL.windowResizable       = True
                , SDL.windowInitialSize     = SDL.V2 1040 720
                }
    window    <- managed $ bracket (SDL.createWindow "Chopaan Grid Playground" cfg) SDL.destroyWindow
    glContext <- managed $ bracket (SDL.glCreateContext window) SDL.glDeleteContext
    _ <- managed  $ bracket createContext destroyContext
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown
    liftIO $ do
      app <- App feeders
                 <$> newIORef 0
                 <*> newIORef (edgesToGraph (fdEdges (head feeders)))
                 <*> newIORef Map.empty
                 <*> newIORef Map.empty
                 <*> newIORef 0.6
                 <*> newIORef 0.5
                 <*> newIORef 12
                 <*> newIORef (replicate 6 0)
                 <*> newIORef (Flows [(0,0),(0,0),(0,0)] 0)
                 <*> newIORef Nothing
                 <*> newIORef (0,0)
                 <*> newIORef False
      feederToState app (head feeders)
      loop window app

loop :: SDL.Window -> App -> IO ()
loop window app = do
  quit <- pump app
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  -- step the physics: pack graph -> categorified kernel -> settled flows
  cur <- readIORef (appCur app)
  let fd = appFeeders app !! cur
  inp <- packInput app fd
  fl  <- settle inp
  writeIORef (appFlows app) fl
  writeIORef (appXflow app) (concatMap (\(p,q) -> [p,q]) (fEdge fl))

  handleDrag app
  drawGraph app
  controls  app

  render
  openGL3RenderDrawData =<< getDrawData
  SDL.glSwapWindow window
  unless quit (loop window app)

-- Poll SDL, forward to ImGui, track mouse (window-relative, from events) + quit.
pump :: App -> IO Bool
pump app = do
  evs <- pollEventsWithImGui
  forM_ evs $ \e -> case SDL.eventPayload e of
    SDL.MouseMotionEvent m ->
      let SDL.P (SDL.V2 x y) = SDL.mouseMotionEventPos m
      in writeIORef (appMouse app) (fromIntegral x, fromIntegral y)
    SDL.MouseButtonEvent b
      | SDL.mouseButtonEventButton b == SDL.ButtonLeft -> do
          let SDL.P (SDL.V2 x y) = SDL.mouseButtonEventPos b
          writeIORef (appMouse app) (fromIntegral x, fromIntegral y)
          writeIORef (appDown  app) (SDL.mouseButtonEventMotion b == SDL.Pressed)
    _ -> pure ()
  pure (any isQuit evs)
  where
    isQuit e = case SDL.eventPayload e of
      SDL.QuitEvent -> True
      SDL.KeyboardEvent k -> SDL.keyboardEventKeyMotion k == SDL.Pressed
                          && SDL.keysymKeycode (SDL.keyboardEventKeysym k) == SDL.KeycodeEscape
      _ -> False
