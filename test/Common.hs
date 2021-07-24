{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, TypeApplications, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, OverloadedStrings, FlexibleContexts, QuantifiedConstraints, UndecidableInstances #-}
module Common (
  module Test.Hspec,
  module Test.QuickCheck.Checkers,
  module Test.QuickCheck,
  almostEqual) where

import Test.Hspec
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances.Time
import Data.ProtoLens.Arbitrary

import qualified Data.Text as T
import qualified Data.Time as Ti
import Numeric.Compensated
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Affine
import Linear.Matrix
import Linear.Quaternion


import Chopaan.Graph
import Chopaan.Node.NodeId
import Chopaan.Node.Metrics
import Chopaan.Node.Mesh
import Chopaan.Kibbutz.Transactor
import qualified Chopaan.Node.HW as HW
import qualified Chopaan.Node.Components as C
import Chopaan.Kibbutz.KbtzId (KbtzId(..))
import Chopaan.Kibbutz
import Chopaan.Graph.Spider
import Chopaan.Graph.Snapshot

import Chopaan.Ui.Interaction

import qualified Proto.NodeMessageSchema.NodeMessages as NM
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as NM

almostEqual :: (Show a, Eq a, Num a, Ord a) => a -> a -> a -> Expectation
almostEqual eta a b = do
  ((abs $ a - b) < eta) `shouldBe` True


instance (Num a, Arbitrary a, Compensable a) => (Arbitrary (Compensated a)) where
  arbitrary =  (\a -> pure $ add a 0 compensated) =<< arbitrary

instance Arbitrary Watts where
  arbitrary = genericArbitrary

instance Arbitrary WattSeconds where
  arbitrary = genericArbitrary

instance (Arbitrary v) => Arbitrary (Node v) where
  arbitrary = genericArbitrary


instance (Arbitrary e, Arbitrary p) => Arbitrary (Battery e p) where
  arbitrary = genericArbitrary


instance (Arbitrary e, Arbitrary p) => Arbitrary (SensorMetrics e p) where
  arbitrary = genericArbitrary

instance Arbitrary (NodeMAC) where
  arbitrary = do
    let el = ['a'..'z']
    xs <- mapM (\_ -> elements el) [1..6]
    ys <- mapM (\_ -> elements el) [1..6]
    let cpld = fmap (\(a, b) -> [a] <> [b]) $ zip xs ys
    return $ NodeId . T.pack . tail $ foldl (\x y -> x <> ":" <> y) "" cpld

instance Arbitrary (NM.EnergyState) where
  arbitrary = arbitraryMessage

instance Arbitrary (NM.RuntimeStats) where
  arbitrary = arbitraryMessage


instance (Arbitrary n, Arbitrary v) => Arbitrary (SnapshotNode n v) where
  arbitrary = genericArbitrary

instance (Arbitrary n, Arbitrary l) => Arbitrary (SnapshotLink n l) where
  arbitrary = genericArbitrary

instance Arbitrary NodeVersion where
  arbitrary = (pure . NodeVersion . T.pack) =<< arbitrary

instance Arbitrary MeshNode where
  arbitrary = genericArbitrary

instance Arbitrary RxSignal where
  arbitrary = genericArbitrary

instance Arbitrary Role where
  arbitrary = genericArbitrary

instance (Arbitrary n) => Arbitrary (TxStatus' n) where
  arbitrary = genericArbitrary

instance (Arbitrary n) => Arbitrary (Stake' n) where
  arbitrary = genericArbitrary

instance (Arbitrary n, Arbitrary v, Arbitrary e) => Arbitrary (SG' n v e) where
  arbitrary = genericArbitrary

instance (forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (k n a b)) => Arbitrary (G k n) where
  arbitrary = genericArbitrary

instance Arbitrary Button where
  arbitrary = genericArbitrary

instance Arbitrary PointerType where
  arbitrary = genericArbitrary

instance Arbitrary Pointer where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (V2 a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (V3 a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (V4 a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (Quaternion a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (Point V2 a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (Point V3 a) where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (M44 a) where
  arbitrary = genericArbitrary
