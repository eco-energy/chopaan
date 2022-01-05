{-# LANGUAGE OverloadedStrings, StandaloneDeriving, DeriveAnyClass, FlexibleInstances, TypeSynonymInstances #-}
module FSSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.State
import Test.QuickCheck.Arbitrary.Generic

import Data.Bifunctor
import qualified Data.Map.Strict as M
import Common

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.FS
import Chopaan.Node.NodeId
import Chopaan.Node.HW
import Chopaan.Node.Components
import qualified Chopaan.Graph.Algebraic as AG
import Data.IORef
import qualified Data.Text as Text

instance Arbitrary (Tag KbtzName) where
  arbitrary = (Tag . KbtzId . Text.pack . getPrintableString) <$> arbitrary

instance Arbitrary (Tag NodeIdx) where
  arbitrary = (Tag . NodeId . getPositive) <$> arbitrary

instance Arbitrary (NodeIdx) where
  arbitrary = (NodeId . getPositive) <$> arbitrary

instance Arbitrary KbtzEv where
  arbitrary = genericArbitrary 

instance (Arbitrary e, Arbitrary v) => Arbitrary (AG.Graph e v) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (Ownership a) where
  arbitrary = genericArbitrary

instance Arbitrary (NodeModel) where
  arbitrary = genericArbitrary

instance Arbitrary BatteryType where
  arbitrary = genericArbitrary
  
instance (Arbitrary a) => Arbitrary (BatteryConf a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (BatteryTop a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (LoadConf a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (LoadTop a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (PVConf a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (PVTop a) where
  arbitrary = genericArbitrary


instance Arbitrary (HW Double) where
  arbitrary = genericArbitrary

spec :: Spec
spec = do
  describe "Kbtz Creation-Deletion events" $ do
    prop "CreateKbtz event adds an empty graph to map" $ \ev ks -> do
      let
        k = M.mapKeys unTag ks
        k' = onKbtzEv ev k 
      case ev of
        CreateKbtz dk -> (M.keys $ k' `M.difference` k) `shouldBe` [unTag dk] 
        DeleteKbtz dk -> (M.keys $ k `M.difference` k') `shouldBe` [unTag dk]
