{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module SubscriberSpec (spec) where

import Chopaan.Subscriber
import Test.Hspec
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import Streamly

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM.TChan as TChan

{--
instance (Show a) => Show (StreamMap SerialT IO Int a) where
  show (StreamMap s) = "This Guy Yo"
--}

spec :: Spec
spec = do
  describe "The Subscriber is a broadcast chan from which subscription streams can be created" $ do
    it "reading from a subscriber produces an infinite stream that can be evaluated by taking n items from it" $ do
      sub <- mkSub
      let
        writer = S.fromList $ zip [1..5 :: Int] [5..10 :: Int]
      dchan <- atomically $ TChan.dupTChan (runSubscriber sub)
      let
        s1 = subStream dchan (\(n, _)-> n == (1 :: Int))
      S.drain $ writeSub sub writer
      a <- S.sum $ S.take 1 s1
      a `shouldBe` 5
    it "creating a subMap should return a map of streams each of which is individually readable by taking n items" $ do
      sub <- mkSub
      let
        ns = [1..5 :: Int]
        writer = S.fromList $ zip (cycle ns) [(5:: Int)..100]
      forkIO $ S.drain $ writeSub sub writer
      smap <- subMap ns sub
      let
        fstream :: Serial Int
        fstream = getStream smap (head ns)
        -- foldrM :: Monad m => (a -> mb -> mb) -> m b -> SerialT m a -> m b
      a <- S.toList $ S.scan FL.sum (S.take 5 fstream)
      --let (Just (a, rest)) = h
      a `shouldBe` ([0, 5, 15, 30, 50, 75 :: Int])
{--
it "StreamMap is a functor" $ do
verboseBatch (functor  (undefined :: StreamMap SerialT IO Int (Int, Int, Int)) )
--}
