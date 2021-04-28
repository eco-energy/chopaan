{-# LANGUAGE OverloadedStrings, TypeFamilies, GeneralizedNewtypeDeriving, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GraphSpec (spec) where

import Control.Applicative ((<$>), (<*>))
import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Foldable (toList)

import Data.Monoid (mempty, (<>))
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Function ((&))
import Test.Hspec
import Data.Greskell.Greskell (toGremlin)
import Data.Greskell.Binder (Binder, newBind, runBinder)
import Data.Greskell.GTraversal ((&.), ($.), liftWalk, gDrop, source, sV')
import Network.Greskell.WebSocket
  ( connect, close, submit, submitPair, submitRaw,
    slurpResults, drainResults
  )

import Control.Exception (bracket)

import Chopaan.Kibbutz.KbtzId
import Chopaan.Graph

spec :: Spec
spec = do
  describe "Chopaan's static configuration is a graph structure" $ do
    it "adding and fetching kibbutzim" $ do
      let
        k = KbtzId "What"
        n = ANode "ab:cd:ef:gh:ij:kl"
        addSB = runBinder $ addKbtz k
        readSB = runBinder $ gGetKbtzByKbtzId k
        readNSB = runBinder $ gKbtzNodes k
        addNToB = runBinder $ addNode k n
      toGremlin (fst addSB)
        `shouldBe` "g.addV(\"kbtz\").property(\"@kbtz_id\",__v0)"
      (toGremlin $ allKbtz &. (liftWalk $ fst readSB))
        `shouldBe` "g.V().hasLabel(\"kbtz\").has(\"@kbtz_id\",__v0)"
      toGremlin (allKbtz &. fst readNSB)
        `shouldBe`
        "g.V().hasLabel(\"kbtz\").has(\"@kbtz_id\",__v0).out(\"kbtzIncludes\")"
      toGremlin (fst addNToB)
        `shouldBe`
        "g.V().hasLabel(\"kbtz\").has(\"@kbtz_id\",__v1).sideEffect(__.addV(\"knode\").property(\"@knode_id\",__v0)).addE(\"kbtzIncludes\").to(__.has(\"@kbtz_id\",__v1).has(\"@kbtz_id\",__v2).out(\"kbtzIncludes\").has(\"@knode_id\",__v3))"

      let (host, port) = ("localhost", 8182)
      bracket (connect host port) close $ \client -> do
        drainResults =<< submit client (gDrop $. liftWalk $ sV' [] $ source "g") Nothing
        drainResults =<< submitPair client addSB
        drainResults =<< submitPair client addNToB
        got_ns <- fmap toList $ slurpResults =<<
                  submitPair client ((allKbtz &. fst readNSB), snd readNSB)
        got_ns `shouldBe` []
        got_k1 <- fmap toList $ slurpResults =<<
                  submitPair client ((allKbtz &. (liftWalk . fst $ readSB)), snd readSB)
        print got_k1
        got_k1 `shouldBe` []
