module NodeSpec (spec) where

import Node
import Test.Hspec
import Test.Hspec.QuickCheck

import Streamly.Prelude as S
import Streamly

import qualified Data.Time as Time


import Import

emptyStream :: (Monad m) => SerialT m EnergyState
emptyStream = S.replicate 100 defaultES 

emptyStream' :: (Monad m) => SerialT m (NodeId Int, EnergyState)
emptyStream' = S.zipWith (,) (S.fromList $ Import.map NodeId [0,1..100]) emptyStream

spec :: Spec
spec = do
  describe "This is how we use node streams" $ do
    let
      initTime = Time.UTCTime (Time.fromGregorian 10 10 2019) 12
      es :: (Monad m) => SerialT m EnergyBalance
      es = S.map snd $ energyStream initTime emptyStream
      ps = S.all (\a-> a == mempty) $ powerStream emptyStream
      zeroNM :: NodeS
      zeroNM = NodeMetrics 0 0 0 0 mempty mempty defaultES
      thisState :: (Monad m) => SerialT m NodeS
      thisState = runNodeMonitor initTime (NodeId 1) emptyStream'
      fs :: (Monad m) => m (Maybe NodeS)
      fs = S.head thisState
    e <- S.foldl' (<>) mempty es
    p <- ps
    strd <- S.sum (stored es)
    l <- S.sum (loss es)
    d <- S.sum (demand es)
    f <- fs
    it "zero energyStream" $ e `shouldBe` mempty 
    it "zero powerStream" $ p `shouldBe` True
    it "zero stored" $ strd `shouldBe` 0
    it "zero loss" $ l `shouldBe` 0
    it "zero demand" $ d `shouldBe` 0
    it "run Node Monitor" $ f `shouldBe` (Just $ zeroNM)
    --it "loss 2" $ (S.foldl (<>) initEA id es) `shouldBe` initEA -- \i -> plus2 i - 2 `shouldBe` i
    --it "lastWait" $ (S.foldl (<>) initEA id es) `shouldBe` initEA -- \i -> plus2 i - 2 `shouldBe`i

