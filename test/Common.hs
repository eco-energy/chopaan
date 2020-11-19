module Common (
  module Test.Hspec,
  module Test.QuickCheck.Checkers,
  module Test.QuickCheck,
  almostEqual) where

import Test.Hspec
import Test.QuickCheck.Checkers
import Test.QuickCheck


almostEqual :: (Show a, Eq a, Num a, Ord a) => a -> a -> a -> Expectation
almostEqual eta a b = do
  ((abs $ a - b) < eta) `shouldBe` True
