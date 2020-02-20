{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module RegistrySpec (spec) where

import Registry
import Test.Hspec
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

import qualified Streamly.Prelude as S
import Streamly

import Lens.Micro ()
import Data.ProtoLens.Arbitrary

import Lens.Micro
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Time as Time
import Node (NodeId(..))
import qualified Data.Text as Text

instance Arbitrary ThingName where
  arbitrary = arbitrary

instance Arbitrary NodeT where
  arbitrary = NodeId <$> arbitrary

instance Arbitrary Kibbutz where
  arbitrary = arbitrary


spec :: Spec
spec = do
  describe "The registry deals with topics and kibbutz configuration" $ do
    let mac = Text.pack "cc:50:e3:a8:69:c4"
    let badMac = Text.pack "cc:50:e3:a8:69:c4"
    it "toControlTopic and fromControlTopic should be inverses of each other" $ do
      let t m = (unNodeId <$> (fromControlTopic . controlTopic $ ((NodeId m) :: NodeT))) `shouldBe` (Just m)
      mapM_ t [mac, badMac]
    it "stateTopic and fromStateTopic should be inverses of each other" $ do
      let t m = (unNodeId <$> (fromStateTopic . stateTopic $ ((NodeId m) :: NodeT))) `shouldBe` (Just m)
      mapM_ t [mac, badMac]
    --it "nameToTopic and topicToNodeId are inverses of each other" $ do
    it "(unNodeId . fromControlTopic . controlTopic $ n) `shouldBe`  " $ do
      let t m = ((unNodeId <$> (fromControlTopic . controlTopic $ ((NodeId m) :: NodeT))) :: Maybe ThingName)
                `shouldBe`
                ((unNodeId <$> (fromStateTopic . stateTopic $ ((NodeId m) :: NodeT))) :: Maybe ThingName)
      mapM_ t [mac, badMac]
