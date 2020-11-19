{-# LANGUAGE TypeApplications #-}
module StorageSpec where

import Chopaan.Node.Storage
import Numeric.Estimator
import Common

instance (Arbitrary a) => Arbitrary (BatteryParams a) where
  arbitrary = BatteryParams
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance (Arbitrary a) => Arbitrary (StateVector a) where
  arbitrary = StateVector
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance (Arbitrary a) => Arbitrary (SensorVector a) where
  arbitrary = SensorVector
    <$> arbitrary
    <*> arbitrary


  
instance (Eq a) => EqProp (BatteryParams a) where
  a =-= b = eq a b
  


spec :: Spec
spec = do
  describe "Trivial cases for process model" $ do
    it "processModel: on 0s." $ do
      let
        bp :: BatteryParams Double
        bp = pure 0
        stateV, stateNoiseV :: StateVector Double
        stateV = pure 0
        stateNoiseV = pure 0
        sensorV, sensorNoiseV :: SensorVector Double
        sensorV = pure 0
        sensorNoiseV = pure 0
        dt = 0 :: Double
      (_, (a, KalmanFilter state var)) <- runKalmanState dt stateV $ runProcessModel bp dt stateNoiseV sensorNoiseV sensorV
      a `shouldBe` 0 
      isNaN <$> state `shouldBe` (pure True)
      (fmap.fmap) isNaN var `shouldBe` (pure.pure $ True)
    it "processModel: on 1s." $ do
      let
        bp :: BatteryParams Double
        bp = pure 1
        stateV, stateNoiseV :: StateVector Double
        stateV = pure 1
        stateNoiseV = pure 1
        sensorV, sensorNoiseV :: SensorVector Double
        sensorV = pure 1
        sensorNoiseV = pure 1
        dt = 1 :: Double
      (_, (a, KalmanFilter state _)) <- runKalmanState dt stateV $ runProcessModel bp dt stateNoiseV sensorNoiseV sensorV
      a `shouldBe` 1
      soC state `shouldBe` 0 -- z_next is 1 - ((1 / 1) * 1) = 0
      hysteresisVoltage state `shouldBe` 1
      diffusionCurrent state `shouldBe` 1
    it "soC-Ocv Conversion tests " $ do
      let
        bp = defBatteryParams{chargeCapacity=10}
        initOCV = ocvToSoC bp 12.06
        processModel = undefined
      almostEqual 0.0005 initOCV (10 * 0.3)
      
      
      -- expTerm = exp (- (abs (1 * 1 * 1 * (1 / 1)))) == - e
      -- sgn = sgn 1 == 1
      -- h_kn = (e - 1 - e)
      -- diffusionCurrent 
