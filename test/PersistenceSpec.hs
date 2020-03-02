module PersistenceSpec where

import Persistence (Timespan, getSpanTimestamps, )
import Test.Hspec
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Instances.Time

dates :: Gen Timespan
dates = infiniteListOf . arbitrary

spec :: Spec
spec = do
  describe "Streams and the filesystem" $ do
    it "1) saving requires a base directory and a date" $ do
      s <- dates
      getSpanTimestamps s `shouldBe` (max s, min s)
    it "reading and writing is possible and doesn't break" $ do
      let eqprop d = (read =<< (flip write $ d) =<< (toPath . getSpanTimestamps) <$> d)
      d <- dates
      (eqprop d) `shouldBe` d 
    -- it "should be able to construct an iterator over the persistence by timespan" $ do
      -- <- (pathsAtInterval . getTimeSpan) <$> dates
