{-# LANGUAGE OverloadedStrings, TypeFamilies, GeneralizedNewtypeDeriving, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedLists #-}
module GraphSpec (spec) where

import Data.Foldable (toList)
import Data.Text (pack)
import qualified Data.Aeson as Aeson
import Test.Hspec
import Data.Greskell.Greskell (toGremlin)
import Data.Greskell.GraphSON
import Data.Greskell.GraphSON.GValue
import Data.Greskell.Binder (runBinder)
import Data.Greskell.GTraversal (($.), liftWalk, gDrop, source, sV')
import Network.Greskell.WebSocket
  ( connect, close, submit, submitPair,
    slurpResults, drainResults
  )
import qualified TestContainers as TC
import qualified TestContainers.Hspec as TC
import Control.Exception (bracket)

import Common
import Chopaan.Kibbutz.KbtzId
import Chopaan.Graph.Kbtz
import Chopaan.Node.HW
import Chopaan.Node.Components



spec :: Spec
spec = do
  gremlinSpec
  integrationSpec

k = KbtzId "What"
kb = AKbtz k
n = "ab:cd:ef:gh:ij:kl"
an = ANode n
hw :: HW Double
hw = HW (SingBC defBC) (SingPC defPC)  (SingLC defLC)


gremlinSpec :: Spec
gremlinSpec = do
  describe "Chopaan's static configuration is a graph structure in gremlin" $ do
    it "adding a kibbutz works" $ do
      let
        writeKbtz = runBinder $ addKbtz' kb
      toGremlin (fst writeKbtz)
        `shouldBe`
        "g.addV(\"kbtz\").property(\"@kbtz_id\",__v0).property(\"@node_type\",\"k\")"

    it "reading a kibbutz by id works" $ do
      let readKbtz = runBinder $ getKbtzById' k
      (toGremlin . fst $ readKbtz)
        `shouldBe`
        "g.V().has(\"@node_type\",\"k\").hasLabel(\"kbtz\").has(\"@kbtz_id\",__v0).valueMap()"

    it "adding a node works" $ do
      let
        writeHH = runBinder $ addHH' an
      toGremlin (fst writeHH)
        `shouldBe`
        "g.addV(\"hh\").property(\"@hh_id\",__v0).property(\"@node_type\",\"h\")"

    it "reading a node by id works" $ do
      let readHH = runBinder $ getHHById n
      (toGremlin . fst $ readHH)
        `shouldBe`
        "g.V().has(\"@node_type\",\"h\").hasLabel(\"hh\").has(\"@hh_id\",__v0).valueMap()"

integrationSpec :: Spec
integrationSpec = do
  aroundAll (TC.withContainers (runJanus "graphSpec")) $ describe "Janusgraph roundtrips" $ do
    it "round-tripping a Kbtz works" $ \(host, port) -> do
      bracket (connect host port) close $ \client -> do
        drainResults =<< submit client (gDrop $. liftWalk $ sV' [] $ source "g") Nothing
        addKbtz client k
        got_k1 <- fmap toList $ slurpResults =<<
                  (submitPair client (runBinder $ getKbtzById' k))
        got_k1 `shouldBe` [kb]

    it "round-tripping a HH works" $ \(host, port) -> do
      bracket (connect host port) close $ \client -> do
        drainResults =<< submit client (gDrop $. liftWalk $ sV' [] $ source "g") Nothing
        let x = (runBinder $ addHH' an)
        drainResults =<< submitPair client x
        got_n1 <- fmap toList $ slurpResults =<<
                  (submitPair client (runBinder $ getHHById n))
        got_n1 `shouldBe` [an]

    it "a hh belonging to a kbtz can be fetched by searching along an edge" $ \(host, port) -> do
      bracket (connect host port) close $ \client -> do
        drainResults =<< submit client (gDrop $. liftWalk $ sV' [] $ source "g") Nothing
        addKbtz client k
        addHHToKbtz client k an
        got_e1 <- getKbtzNodes client k
        got_e1 `shouldBe` [n]


    it "a hwconfig belonging to an hh can be fetched by searching along an edge" $ \(host, port) -> do
      bracket (connect host port) close $ \client -> do
        drainResults =<< submit client (gDrop $. liftWalk $ sV' [] $ source "g") Nothing
        addKbtz client k
        addHHToKbtz client k an
        addHWToHH client n hw
        got_h1 <- getNodeHW client n
        got_h1 `shouldBe` [hw]
            
    it "the last sync date always returns the latest and there's only ever one node" $ \(host, port) -> do
      bracket (connect host port) close $ \client -> do
        drainResults =<< submit client (gDrop $. liftWalk $ sV' [] $ source "g") Nothing
        addKbtz client k
        addHHToKbtz client k an
        let ls = fmap (\i -> pack $ "ThisLastSync-" <> (show i)) [1..10]
        mapM_ (addLastSyncToHH client n) ls
        got_h1 <- getNodeLastSync client n
        got_h1 `shouldBe` [head . reverse $ ls]
