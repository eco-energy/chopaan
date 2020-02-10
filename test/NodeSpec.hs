module NodeSpec (spec) where

import Node
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck
import Test.QuickCheck.Instances.Time

import Streamly.Prelude as S
import Streamly

import qualified Data.Time as Time
import Import
import Proto.NodeMessages
import Proto.NodeMessages_Fields
import Lens.Micro
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Arbitrary

instance Arbitrary EnergyState where
  arbitrary = arbitraryMessage

instance (Arbitrary a) => Arbitrary (Power a) where
  arbitrary = Power <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary 

instance (Arbitrary a) => Arbitrary (Energy a) where
  arbitrary = Energy <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance (Arbitrary a, Arbitrary b) => Arbitrary (NodeMetrics a b) where
  arbitrary = NodeMetrics <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "This is how we use node streams" $ do
    it "run Node Monitor" $ do
      let
        tES :: EnergyState
        tES = defMessage
          & batteryVoltage .~ 12.0
          & gridVoltage .~ 60.0
          & batteryToLoadCurrent .~ 10.0
          & batteryToGridCurrent .~ 10.0
          & gridToBatteryCurrent .~ 0.0
          & solarInputCurrent .~ 10.0
          & dutyCycle .~ 3.0
          & cpuTime .~ (100)
        stream :: (IsStream t, Monad m) => t m (NodeId Int, EnergyState)
        stream = S.repeat ((NodeId 10), tES)

    --it "loss 2" $ (S.foldl (<>) initEA id es) `shouldBe` initEA -- \i -> plus2 i - 2 `shouldBe` i
    --it "lastWait" $ (S.foldl (<>) initEA id es) `shouldBe` initEA -- \i -> plus2 i - 2 `shouldBe`i

