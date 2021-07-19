{-# LANGUAGE OverloadedStrings #-}
module ThreeDSpec where

import Test.Hspec
import Chopaan.Ui.ThreeD
import Chopaan.Ui.Interaction
import qualified Linear.Vector as V
import Linear.V4
import Linear.V3
import Linear.V2
import Linear.Metric
import Linear.Matrix
import Control.Lens (over)

import Test.QuickCheck

spec = describe "3D in Shpadoinkle for CSS transforms" $ do
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
    print $ cameraCSSMat c
    print $ (cameraCSSMat c) <> (translatePx (2500 / 2) (1700 / 2))
    (hasNaN . matrixWorldInverse $ c) `shouldBe` False
    (hasNaN . projectionTransform $ c) `shouldBe` False
  it "Zoom in on ascending pointer pos, Zoom out on descending" $ do
    let pvs = (\i -> (i, i) :: PointerPos) <$> [1..(10 :: Double)]
    let x = foldl (flip zoomA) (V.zero, V.zero) pvs
    (ydiff x) `shouldBe` 1.0
    
    
