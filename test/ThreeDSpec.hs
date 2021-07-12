{-# LANGUAGE OverloadedStrings #-}
module ThreeDSpec where

import Test.Hspec
import Chopaan.Ui.ThreeD
import qualified Linear.Vector as V
import Linear.V4
import Linear.Matrix
import Control.Lens (over)


spec = describe "3D in Shpadoinkle for CSS transforms" $ do
  it "cssMat right identity matrix" $ do
    let mat = (identity :: M44R)
    cssMat mat `shouldBe` "matrix3d(1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0)"
  it "column major ordering for output" $ do
    let mat12 = over _x (over _y (+1)) $ (pure V.zero)
    cssMat mat12 `shouldBe` "matrix3d(0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)"
  it "No NaNs in inverting objects" $ do
    let c = (defCam 3000 4878 2555)
    (hasNaN . matrixWorldInverse $ c) `shouldBe` False
    (hasNaN . projectionTransform $ c) `shouldBe` False
