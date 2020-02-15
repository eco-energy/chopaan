module SoCSpec where

import SoC
import Test.Hspec
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck


instance (Arbitrary a) => Arbitrary (SoCParams a) where
  arbitrary = SoCParams <$> arbitrary <*> arbitrary <*> arbitrary
  
instance (Eq a) => EqProp (SoCParams a) where
  a =-= b = eq a b


spec :: Spec
spec = do
  describe "SoC has to be estimated from a node stream" $ do
    it "" $ do
      verboseBatch (applicative (undefined :: SoCParams (Int, Int, Int)))
