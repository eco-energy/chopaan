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
      forAll genWheelD $ \wd ->
      let
        zs = mkWheels wd
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
    it "Zoom Should Transform a Camera and its reverse should get it back to where it was" $ property $
      prop_zoom_in_out_camera
    -- it "Rotations work" $ do
    --   xs <- arbs @CurPos 100
    -- it "Rotations on a point are invertible" $ property $
    --   prop_rotate_invertible
    it "Panning on a point is invertible" $ property $
      prop_pan_invertible
      
applyIs :: [Interact] -> T
applyIs = foldMap (evalI)

arbMat :: Gen M44R
arbMat = arbitrary

wheelPoint = (,) <$> genWheelD <*> (arbitrary @(V3 Double))

pathPoint = (,)
            <$> path --
            <*> (arbitrary @(V3 Double)) -- 

prop_pan_invertible :: Property
prop_pan_invertible = forAll pathPoint $ \(pvs, p) ->
  nearZero $ pointIsoStateful p $ isoPan pvs

-- prop_rotate_invertible' :: Property
-- prop_rotate_invertible' = forAll pathPoint $ \(pvs, p) ->
--   nearZero $ camIso (defCam p 4878 2555) $ isoRotate pvs

prop_rotate_invertible :: Property
prop_rotate_invertible = forAll pathPoint $ \(pvs, p) ->
  nearZero $ pointIsoStateful p $ isoRotate pvs

fseq = flip seq

isoRotate :: [CurPos] -> (T, T)
isoRotate [] = (Endo id, Endo id)
isoRotate (x:[]) = (Endo id, Endo id)
isoRotate ds = let
  fw = mkPointers (RotateB : repeat NoneB) ds
  rv = mkPointers (RotateB : repeat NoneB) $ reverse ds
  rstart = initPosState (pure 0)
  c = contScanPure pointerControl (zoomI rstart) fw
  c' = contScanPure pointerControl (zoomI rstart) rv
  ft = applyIs c
  rt = applyIs c'
  deb = unsafePerformIO (do
                            print (fmap (getState) c)
                            print (fmap (getState) c')
                            --print $ (appEndo rt) (appEndo ft identity)
                            --print $ 
                            )
  in (ft, rt) --`fseq` deb


isoPan :: [CurPos] -> (T, T)
isoPan [] = (Endo id, Endo id)
isoPan (_:[]) = (Endo id, Endo id)
isoPan ds = let
  fw = mkPointers (PanB : repeat NoneB) ds
  rv = mkPointers (PanB : repeat NoneB) $ reverse ds
  c = contScanPure pointerControl (panI $ initPosState (head ds)) fw
  c' = contScanPure pointerControl (panI $ initPosState (head $ reverse ds)) rv
  ft = applyIs c
  rt = applyIs c'
  deb = unsafePerformIO (do
                            print $ fmap (fst . getState) c
                            print $ reverse $ fmap (fst . getState) c'
                            print $ fmap (snd . getState) c
                            print $ fmap (snd . getState) c'
                            --print $ zipWith (V.^-^) (fmap (snd . getState) c) (reverse $ fmap (snd . getState) c')
                           )
  in (ft, rt) `fseq` deb


pointIsoStateful :: V3 R -> (T, T) -> V3 R
pointIsoStateful p (ft, rt) = let
  fw = (appEndo ft identity)
  bw = (appEndo rt identity)
  p' = fw !* (Linear.V4.point p)
  p'' = (bw !* p') ^. _xyz
  res = (p V.^-^ p'')
  deb = unsafePerformIO (do
                            case (nearZero res) of
                              True -> return ()
                              False -> do
                            --print $ p
                            --print $ p'
                            --print $ p''
                                print fw
                                print bw
                            --print $ res
                                --print $ (appEndo rt $ appEndo ft identity)
                            )
  in res `fseq` deb

pointIso :: V3 R -> (T, T) -> V3 R
pointIso p (ft, rt) = let
  p' = (appEndo ft identity) !* (point p)
  p'' = ((appEndo rt identity) !* p') ^. _xyz
  res = (p V.^-^ p'')
  deb = unsafePerformIO (do
                            case (nearZero res) of
                              True -> return ()
                              False -> do
                                print $ res
                                print $ appEndo rt $ appEndo ft identity
                            )
  in res --`fseq` deb

camIso :: Camera -> (T, T) -> M44R
camIso cam (ft, rt) = let
  cam' = transformCamera ft cam
  cam'' = transformCamera rt cam'
  res = ((matrixWorldInverse $ cam) V.^-^ (matrixWorldInverse $ cam''))
  in res

prop_zoom_in_out :: Property
prop_zoom_in_out = forAll wheelPoint $ \(wd, p) ->
  nearZero $ pointIso p $ isoZoom wd

      
prop_zoom_in_out_camera :: Property
prop_zoom_in_out_camera = forAll wheelPoint $ \(wd, p) ->
  nearZero $ camIso (defCam p 4878 2555) $ isoZoom wd
      
isoZoom :: [V2 R] -> (T, T)
isoZoom [] = (Endo id, Endo id) 
isoZoom ds = let
  fw = mkWheels ds
  rv = mkWheels (fmap (V.^* (-1)) ds)
  rstart = initPosState (pure 0)
  c = contScanPure wheelControl (zoomI rstart) fw
  c' = contScanPure wheelControl (zoomI rstart) rv
  ft = applyIs c
  rt = applyIs c'
  in (ft, rt)
  -- `fseq` pr
  -- where
  -- pr = unsafePerformIO (do
  --                          case (nearZero res) of
  --                            True -> return ()
  --                            False -> do
  --                              print $ zipWith (V.^+^) (fmap wheelDelta fw) (fmap wheelDelta rv)
  --                              print $ zipWith (V.^+^) (fmap (snd . getState) c) (fmap (snd . getState) c')

  --                              print $ p'
  --                              print $ p''
  --                              print res
  --                              --print $ zd c c'


prop_cont_over_sum :: Property
prop_cont_over_sum = forAll path $ \pvs ->
      let
        ps = mkPointers (PanB : repeat NoneB) pvs
        rs = mkPointers (RotateB : repeat NoneB) pvs
        c = contScanPure pointerControl (panI $ initPosState (head pvs)) (ps <> rs)
        p' = contScanPure pointerControl (panI $ initPosState (head pvs)) ps
        r' = contScanPure pointerControl (rotateI $ initPosState (head pvs)) rs
      --in ((take (length pvs) c) `shouldBe` p') <>
       --  ((drop (length pvs) $ c) `shouldBe` r')
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

genWheelD :: Gen [V2 R]
genWheelD = arbitrary

mkPointers :: [Button] -> [CurPos] -> [Pointer]
mkPointers bs ps = (\(b, p) -> Pointer p (Just Mouse) 1 b) <$> (zip bs ps)

mkWheels :: [V2 R] -> [Wheel]
mkWheels = (fmap getWheelZ)

getWheelZ :: V2 R -> Wheel
getWheelZ d = Wheel PixelDelta d

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = (zip xs (tail xs))
  where
    zip' [] (b:[]) = [(b, b)]
    zip' (a:[]) [] = [(a, a)]
    zip' (a:as) (b:bs) = (a, b) : zip' as bs


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
