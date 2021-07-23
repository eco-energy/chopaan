{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ExplicitForAll, TypeApplications, FlexibleContexts #-}
module ThreeDSpec where

import Test.Hspec
import Data.Semigroup
import Shpadoinkle (runContinuation, Continuation)
import Chopaan.Ui.Base
import Chopaan.Ui.ThreeD
import Chopaan.Ui.Interaction
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
    ((scanZ pvs) ^. _2 . _y) `shouldBe` 1.0
  it "Continuations are Isomorphic to monadic folds" $ do
    c <- contZ getButtonZ pvs
    c `shouldBe` (scanZ pvs)
    p <- contP getButtonP pvs
    print p
    p `shouldBe` (scanP pvs)
  it "Continuations only work for their own buttons" $ do
    zoomable <- contZ getButtonZ pvs
    panable <- contP getButtonP pvs
    noContZ <- contZ getButtonN pvs
    noContP <- contP getButtonN pvs
    noContZ `shouldBe` (initPosState V.zero)
    noContP `shouldBe` (initPanState V.zero)
    zoomable `shouldNotBe` noContZ
    panable `shouldNotBe` noContP
  --it "Continuations should stop when "
    
pvs :: [CurPos]
pvs = (\i -> toPos (i, i)) <$> [1..(10000 :: Double)]

scanZ :: (Foldable f) => f CurPos -> ZoomS
scanZ = foldl (flip zoomA) (initPosState V.zero)

scanP :: (Foldable f) => f CurPos -> PanS
scanP = foldl (flip panA) (initPanState V.zero)

scanR :: (Foldable f) => f CurPos -> RotateS
scanR = foldl (flip rotateA) (initRotateState V.zero)

getButtonZ p = Pointer p (Just Mouse) 1 (ZoomB)
getButtonN p = Pointer p (Just Mouse) 1 (NoneB)
getButtonR p = Pointer p (Just Mouse) 1 (RotateB)
getButtonP p = Pointer p (Just Mouse) 1 (PanB)

contNext :: forall m a. Monad m
      => (CurPos -> Pointer)
      -> (Pointer -> Continuation m a)
      -> a
      -> CurPos
      -> m (a -> a)
contNext f c init p = runContinuation (c (f p)) init

zoomNext :: Monad m => (CurPos -> Pointer) -> CurPos -> m (ZoomS -> ZoomS)
zoomNext f = contNext f zoomC (initZoomState V.zero)

panNext :: Monad m => (CurPos -> Pointer) -> CurPos -> m (PanS -> PanS)
panNext f = contNext f panC (initPanState V.zero)

rotateNext :: Monad m => (CurPos -> Pointer) -> CurPos -> m (RotateS -> RotateS)
rotateNext f = contNext f rotateC (initRotateState V.zero)

contF :: forall m a. MonadAsync m
      => (CurPos -> Pointer)
      -> ((CurPos -> Pointer) -> CurPos -> m (a -> a))
      -> a
      -> [CurPos]
      -> m a
contF f cont init ps = S.foldlM' (\prev cp -> do
                                    n <- cont f cp
                                    return (n prev)) init $ S.fromList ps

contZ f = contF f zoomNext (initZoomState V.zero)
contP f = contF f panNext (initPanState V.zero)
contR f = contF f rotateNext (initRotateState V.zero)
