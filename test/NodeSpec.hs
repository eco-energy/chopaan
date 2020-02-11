module NodeSpec (spec) where

import Node
import Test.Hspec
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

import qualified Streamly.Prelude as S
import Streamly

import qualified Data.Time as Time
import Proto.NodeMessages ()
import Proto.NodeMessages_Fields ()
import Lens.Micro ()
import Data.ProtoLens.Arbitrary

import Data.Semigroup (Product(..))

instance Arbitrary EnergyState where
  arbitrary = arbitraryMessage

instance (Arbitrary a) => Arbitrary (Power a) where
  arbitrary = Power <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary 

instance (Arbitrary a) => Arbitrary (Energy a) where
  arbitrary = Energy <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance (Arbitrary a, Arbitrary b) => Arbitrary (NodeMetrics a b) where
  arbitrary = NodeMetrics <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance (Eq a) => EqProp (Power a) where
  a =-= b = eq a b

instance (Eq a) => EqProp (Energy a) where
  a =-= b = eq a b


spec :: Spec
spec = do
  describe "This is how we use node streams" $ do
    it "power is a monoid and an applicative" $ do
      verboseBatch (monoid (undefined :: (Power Int)))
      verboseBatch (applicative (undefined :: Power (Int, Int, Int)))
    it "energy is a monoid and an applicative" $ do
      verboseBatch (monoid (undefined :: (Energy Int)))
      verboseBatch (applicative (undefined :: Energy (Int, Int, Int)))
    --it "run Node Monitor" $ do
    --  1 `shouldBe` 2
