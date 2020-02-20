{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module SubscriberSpec (spec) where

import Subscriber
import Test.Hspec
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

import qualified Streamly.Prelude as S
import Streamly

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)

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
      s1 <- subStream sub (\(n, _)-> n == (1 :: Int))
      S.drain $ writeSub sub writer
      a <- S.sum $ S.take 1 s1
      a `shouldBe` 5
{--
it "StreamMap is a functor" $ do
verboseBatch (functor  (undefined :: StreamMap SerialT IO Int (Int, Int, Int)) )
--}
