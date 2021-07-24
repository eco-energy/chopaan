{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ExplicitForAll, TypeApplications, FlexibleContexts #-}
module ThreeDSpec where

import Test.Hspec
import Data.Semigroup
import Shpadoinkle (runContinuation, Continuation)
import Chopaan.Ui.Base
import Chopaan.Ui.ThreeD
import Chopaan.Ui.Interaction

import Linear.Affine
import qualified Linear.Vector as V
import Linear.V4
import Linear.V3
import Linear.V2
import Linear.Metric
import Linear.Matrix
import Control.Lens

import qualified Streamly.Prelude as S
import Streamly

import Test.QuickCheck

spec = do
  describe "3D Geometery for Shpadoinkle CSS transforms" $ do
    it "cssMat right identity matrix" $ do
      let mat = (identity :: M44R)
      cssMat mat `shouldBe` "matrix3d(1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0)"
    it "column major ordering for output" $ do
      let mat12 = over _x (over _y (+1)) $ (pure V.zero)
      cssMat mat12 `shouldBe` "matrix3d(0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)"
    it "No NaNs in inverting objects" $ do
      let
        o = V3 0 0 3000
        c = (defCam o 4878 2555)
      (hasNaN . matrixWorldInverse $ c) `shouldBe` False
      (hasNaN . projectionTransform $ c) `shouldBe` False

  describe "Various Interactions And Their Invariants" $ do
    it "Zoom in on ascending pointer pos, Zoom out on descending" $ do
      ((scanZ pvs) ^. _2 . _y) `shouldBe` (realToFrac $ length pvs)
    it "Continuations are Isomorphic to monadic folds" $ do
      c <- contZ (uncurry getWheelZ) pvs
      c `shouldBe` (scanZ pvs)
      p <- contP getButtonP pvs
      p `shouldBe` (scanP pvs)
    it "Continuations only work for their own buttons" $ do
      --zoomable <- contZ getWheelZ pvs
      panable <- contP getButtonP pvs
      --noContZ <- contZ getButtonN pvs
      noContP <- contP getButtonN pvs
      --noContZ `shouldBe` zeroZoom
      noContP `shouldBe` zeroPan
      --zoomable `shouldNotBe` noContZ
      panable `shouldNotBe` noContP
    it "control' demuxes via event type equivalently to each constituent cont" $ do
      p <- contP getButtonP pvs
      p' <- contPR getButtonP (panI zeroPan) pvs
      (appEndo (evalI . panI $ p) identity) `shouldBe` (appEndo (evalI p') identity)
    
pvs :: [CurPos]
pvs = (\i -> toPos (i, i)) <$> [1..(10000 :: Double)]

scanZ :: (Foldable f) => f CurPos -> ZoomS
scanZ = foldl (flip zoomA') zeroZoom

scanP :: (Foldable f) => f CurPos -> PanS
scanP = foldl (flip panA) zeroPan

scanR :: (Foldable f) => f CurPos -> RotateS
scanR = foldl (flip rotateA) zeroRotate

getButtonZ p = Pointer p (Just Mouse) 1 (ZoomB)

getWheelZ :: CurPos -> CurPos -> Wheel
getWheelZ p' p = Wheel PixelDelta (p .-. p')

getButtonN p = Pointer p (Just Mouse) 1 (NoneB)
getButtonR p = Pointer p (Just Mouse) 1 (RotateB)
getButtonP p = Pointer p (Just Mouse) 1 (PanB)

contNext :: forall m a b z. Monad m
      => (z -> b)
      -> (b -> Continuation m a)
      -> a
      -> z
      -> m (a -> a)
contNext f c init p = runContinuation (c (f p)) init

zoomNext :: Monad m => ((CurPos, CurPos) -> Wheel) -> (CurPos, CurPos) -> m (ZoomS -> ZoomS)
zoomNext f = contNext f zoomC (zeroZoom)

panNext :: Monad m => (CurPos -> Pointer) -> CurPos -> m (PanS -> PanS)
panNext f = contNext f panC (zeroPan)

rotateNext :: Monad m => (CurPos -> Pointer) -> CurPos -> m (RotateS -> RotateS)
rotateNext f = contNext f rotateC (zeroRotate)

intrNext :: Monad m => Interact -> (CurPos -> Pointer) -> CurPos -> m (Interact -> Interact)
intrNext i f = contNext f control' i

contF :: forall m a b z. MonadAsync m
      => (z -> b)
      -> ((z -> b) -> z -> m (a -> a))
      -> a
      -> [z]
      -> m a
contF f cont init ps = S.foldlM' (\prev cp -> do
                                    n <- cont f cp
                                    return (n prev)) init $ S.fromList ps

contZ :: (MonadAsync m) => ((CurPos, CurPos) -> Wheel) -> [CurPos] -> m (ZoomS)
contZ f xs = contF f zoomNext zeroZoom (z xs)
  where
    z [] = []
    z (x:y:zs) = [(x, y)] <> (z zs)
    
contP f = contF f panNext zeroPan
contR f = contF f rotateNext zeroRotate
contPR f i = contF f (intrNext i) i


zeroZoom = (initZoomState $ pure 0)
zeroPan = (initPanState $ V.zero)
zeroRotate = (initRotateState $ V.zero)
