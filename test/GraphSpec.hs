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

import Control.Exception (bracket)

import Chopaan.Kibbutz.KbtzId
import Chopaan.Graph.Kbtz
import Chopaan.Node.HW
import Chopaan.Node.Components



spec :: Spec
spec = do
  let
    (host, port) = ("localhost", 8182)
  
  afterAll_
    (bracket (connect host port) close $ \client -> do
                drainResults =<< submit client (gDrop $. liftWalk $ sV' [] $ source "g") Nothing) $ do
      describe "Chopaan's static configuration is a graph structure" $ do
        let
            k = KbtzId "What"
            kb = AKbtz k
            n = "ab:cd:ef:gh:ij:kl"
            an = ANode n
            hw :: HW Double
            hw = HW (SingBC defBC) (SingPC defPC)  (SingLC defLC)
        it "adding a kibbutz works" $ do
          let
            writeKbtz = runBinder $ addKbtz' kb
          toGremlin (fst writeKbtz)
            `shouldBe`
            "g.addV(\"kbtz\").property(\"@kbtz_id\",__v0)"

        it "reading a kibbutz by id works" $ do
          let readKbtz = runBinder $ getKbtzById' k
          (toGremlin . fst $ readKbtz)
            `shouldBe`
            "g.V().hasLabel(\"kbtz\").has(\"@kbtz_id\",__v0).valueMap()"

        it "adding a node works" $ do
          let
            writeHH = runBinder $ addHH' an
          toGremlin (fst writeHH)
            `shouldBe`
            "g.addV(\"hh\").property(\"@hh_id\",__v0)"

        it "reading a node by id works" $ do
          let readHH = runBinder $ getHHById n
          (toGremlin . fst $ readHH)
            `shouldBe`
            "g.V().hasLabel(\"hh\").has(\"@hh_id\",__v0).valueMap()"


        it "round-tripping a Kbtz works" $ do
          bracket (connect host port) close $ \client -> do
            drainResults =<< submit client (gDrop $. liftWalk $ sV' [] $ source "g") Nothing
            addKbtz client k
            got_k1 <- fmap toList $ slurpResults =<<
                      (submitPair client (runBinder $ getKbtzById' k))
            got_k1 `shouldBe` [kb]

        it "round-tripping a HH works" $ do
          bracket (connect host port) close $ \client -> do
            drainResults =<< submit client (gDrop $. liftWalk $ sV' [] $ source "g") Nothing
            drainResults =<< submitPair client (runBinder $ addHH' an)
            got_n1 <- fmap toList $ slurpResults =<<
                      (submitPair client (runBinder $ getHHById n))
            got_n1 `shouldBe` [an]


        it "adding an hh to a kbtz allows that hh to be fetched when searching along an edge" $ do
          bracket (connect host port) close $ \client -> do
            drainResults =<< submit client (gDrop $. liftWalk $ sV' [] $ source "g") Nothing
            addKbtz client k
            addHHToKbtz client k an
            got_e1 <- getKbtzNodes client k
            got_e1 `shouldBe` [n]


        it "adding a hwconfig to an hh allows that hh to be fetched when searching along an edge" $ do
          bracket (connect host port) close $ \client -> do
            drainResults =<< submit client (gDrop $. liftWalk $ sV' [] $ source "g") Nothing
            addKbtz client k
            addHHToKbtz client k an
            addHWToHH client n hw
            got_h1 <- getNodeHW client n
            got_h1 `shouldBe` [hw]
            
        it "adding a last sync date always returns the latest and there's only ever one node" $ do
          bracket (connect host port) close $ \client -> do
            drainResults =<< submit client (gDrop $. liftWalk $ sV' [] $ source "g") Nothing
            addKbtz client k
            addHHToKbtz client k an
            let ls = fmap (\i -> pack $ "ThisLastSync-" <> (show i)) [1..10]
            mapM_ (addLastSyncToHH client n) ls
            got_h1 <- getNodeLastSync client n
            got_h1 `shouldBe` [head . reverse $ ls]
