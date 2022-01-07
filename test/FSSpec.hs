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
