{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ExplicitForAll, TypeApplications, FlexibleContexts #-}
module ThreeDSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers hiding (T)

import System.IO.Unsafe
import Control.Monad.Identity

import Data.Semigroup
import Shpadoinkle (runContinuation, Continuation, maybeC')
import Common hiding (T)
import Chopaan.Ui.Base
import Chopaan.Ui.Events
import Chopaan.Ui.ThreeD
import Chopaan.Ui.Interaction

import Linear.Affine
import Linear.Epsilon
import qualified Linear.Vector as V
import Linear.V4
import Linear.V3
import Linear.V2
import Linear.Metric
import Linear.Matrix
import Control.Lens

import qualified Streamly.Prelude as S
import Streamly


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

  describe "Invariants of Interaction Continuations: " $ do
    it "control is nice and works as the composition over Interact's Constructors" $ property $
      prop_cont_over_sum
    it "wheelControl should handle (Maybe Interaction) by initializing its own state" $ property $
      forAll path $ \pvs ->
      let
        zs = mkWheels pvs
        c = contScanPure wheelControl (zoomI $ initPosState (pure 0)) zs
        c' = contScanPure wheelControl' Nothing zs
      in c' == (fmap Just c)
    it "pointerC deals with NoneB events by propagating the active interaction" $ property $
      forAll path $ \pvs ->
        let
          ps = mkPointers (repeat PanB) pvs
          ps' = mkPointers (PanB : (repeat NoneB)) pvs
          c = contScanPure pointerControl (panI $ initPosState (pure 0)) ps
          c' = contScanPure pointerControl (panI $ initPosState (pure 0)) ps'
        in c' == c
    it "Zoom Should Transform an object and its reverse should get it back to where it was" $ property $
      prop_zoom_in_out

applyIs :: [Interact] -> T
applyIs = foldMap (evalI)

arbMat :: Gen M44R
arbMat = arbitrary

pathMatPoint = (,,) <$> path <*> arbMat <*> (arbitrary @(V4 Double))

prop_zoom_in_out :: Property
prop_zoom_in_out = verbose $ forAll (path) $ \(pvs) ->
  nearZero $ (isoZoom pvs)

isoZoom [] = (pure 1) V.^-^ (pure 1) 
isoZoom pvs = let
  s = identity
  p = pure 1
  fw = mkWheels pvs
  rv = mkWheels (reverse pvs)
  rstart [] = zoomI $ initPosState (pure 1)
  rstart (a:_) = zoomI $ initPosState a
  c = contScanPure wheelControl (zoomI $ initPosState (pure 0)) fw
  c' = contScanPure wheelControl (rstart (reverse pvs)) rv
  ft = appEndo (applyIs c) s
  rt = appEndo (applyIs c') s
  pr = unsafePerformIO (do
                      --print fw
                      --print rv
                           case (nearZero res) of
                             True -> return ()
                             False -> do
                               print res
                       )
  p' = ft !* p
  p'' = rt !* p'
  fseq = flip seq
  res = (p V.^-^ p'')
  in res `fseq` pr   

prop_cont_over_sum :: Property
prop_cont_over_sum = forAll path $ \pvs ->
      let
        ps = mkPointers (PanB : repeat NoneB) pvs
        rs = mkPointers (RotateB : repeat NoneB) pvs
        c = contScanPure pointerControl (panI $ initPosState (pure 0)) (ps <> rs)
        p' = contScanPure pointerControl (panI $ initPosState (pure 0)) ps
        r' = contScanPure pointerControl (rotateI $ initPosState (pure 0)) rs
      in ((take (length pvs) c) `leqEpI` p') && ((drop (length pvs) $ c) `leqEpI` r')

nearEq :: forall f a. (V.Additive f, Metric f, Epsilon a) => f a -> f a -> Bool 
nearEq x y = nearZero . quadrance $ x V.^-^ y

leqEpI xs ys = leqEpS (fmap getState xs) (fmap getState ys)--leq dI
  where
    dI x y = (nearEq (x' ^. _1) (y' ^. _1)) && (nearEq (x' ^. _2) (y' ^. _2))
      where
        x' = getState x
        y' = getState y

leqEpS :: forall f g a. (V.Additive f, Metric f, V.Additive g, Metric g, Epsilon a) => [(f a, g a)] -> [(f a, g a)] -> Bool
leqEpS xs ys = let
  (p, d) = (unzip xs)
  (p', d') = (unzip ys)
  in (leqEp p p') && (leqEp d d')
    
leqEp :: forall f a. (V.Additive f, Metric f, Epsilon a) => [f a] -> [f a] -> Bool
leqEp = leq nearEq

leq :: (a -> a -> Bool) -> [a] -> [a] -> Bool
leq f xs ys = foldl (&&) True $ fmap (uncurry f) (zip xs ys) 

path :: Gen [CurPos]
path = arbitrary

mkPointers :: [Button] -> [CurPos] -> [Pointer]
mkPointers bs ps = (\(b, p) -> Pointer p (Just Mouse) 1 b) <$> (zip bs ps)

mkWheels :: [CurPos] -> [Wheel]
mkWheels xs = (uncurry getWheelZ) <$> (pairs xs)

getWheelZ :: CurPos -> CurPos -> Wheel
getWheelZ p' p = Wheel PixelDelta (p .-. p')

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = (zip' xs (tail xs))
  where
    zip' [] (b:[]) = [(b, b)]
    zip' _as [] = []
    zip' (a:as) (b:bs) = (a, b) : zip as bs


contScan :: forall m a b z. Monad m
      => (z -> Continuation m a)
      -> a
      -> [z]
      -> m [a]
contScan cont init ps = S.toList $ S.postscanlM' (\prev cp -> do
                                                n <- runContinuation (cont cp) prev
                                                return (n prev)) init $ S.fromList ps

contScanI = (contScan @Identity)

contScanPure c i p = runIdentity (contScanI c i p)
