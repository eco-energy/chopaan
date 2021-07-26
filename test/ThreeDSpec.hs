{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ExplicitForAll, TypeApplications, FlexibleContexts #-}
module ThreeDSpec where

import Test.Hspec
import Data.Semigroup
import Shpadoinkle (runContinuation, Continuation, maybeC')
import Chopaan.Ui.Base
import Chopaan.Ui.Events
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
  describe "3D Geometery for Shpadoinkle CSS transforms: " $ do
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

  describe "Various Interactions And Their Invariants: " $ do
    it "control is nice and works as the composition over Interact's Constructors" $ do
      let
        ps = mkPointers (PanB : repeat NoneB) pvs
        rs = mkPointers (RotateB : repeat NoneB) pvs
      c <- contScan pointerControl (panI $ initPosState (pure 0)) (ps <> rs)
      p' <- contScan pointerControl (panI $ initPosState (pure 0)) ps
      r' <- contScan pointerControl (rotateI $ initPosState (pure 0)) rs
      print p'
      (take (length pvs) c) `shouldBe` p'
      (drop (length pvs) $ c) `shouldBe` r'
    it "wheelControl should handle (Maybe Interaction) by initializing its own state" $ do
      let
        zs = mkWheels pvs
      c <- contScan wheelControl (zoomI $ initPosState (pure 0)) zs
      c' <- contScan (wheelControl') Nothing zs
      print c'
      c' `shouldBe` (fmap Just c)
    it "pointerC deals with NoneB events by propagating the active interaction" $ do
      let
        ps = mkPointers (repeat PanB) pvs
        ps' = mkPointers (PanB : (repeat NoneB)) pvs
      c <- contScan pointerControl (panI $ initPosState (pure 0)) ps
      c' <- contScan pointerControl (panI $ initPosState (pure 0)) ps'
      c' `shouldBe` c

      
pvs :: [CurPos]
pvs = (\i -> toPos (i, i)) <$> [1..(10 :: Double)]

mkPointers :: [Button] -> [CurPos] -> [Pointer]
mkPointers bs ps = (\(b, p) -> Pointer p (Just Mouse) 1 b) <$> (zip bs ps)

mkWheels :: [CurPos] -> [Wheel]
mkWheels xs = (uncurry getWheelZ) <$> (pairs xs)

getWheelZ :: CurPos -> CurPos -> Wheel
getWheelZ p' p = Wheel PixelDelta (p .-. p')

pairs :: [a] -> [(a, a)]
pairs xs = (zip' xs (tail xs))
  where
    zip' [] (b:[]) = [(b, b)]
    zip' _as [] = []
    zip' (a:as) (b:bs) = (a, b) : zip as bs


contScan :: forall m a b z. MonadAsync m
      => (z -> Continuation m a)
      -> a
      -> [z]
      -> m [a]
contScan cont init ps = S.toList $ S.postscanlM' (\prev cp -> do
                                                n <- runContinuation (cont cp) prev
                                                return (n prev)) init $ S.fromList ps
