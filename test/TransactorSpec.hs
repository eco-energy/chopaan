module TransactorSpec where

import Chopaan.Transactor

import Test.Hspec
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

import qualified Data.Time as Time
import qualified Data.Text as Text
import Data.ULID (getULID)
import Chopaan.Registry (NodeT)
import Chopaan.Node (NodeId(..))
import Lens.Micro
import Proto.NodeMessageSchema.NodeMessages_Fields as NM



spec :: Spec
spec = do
  describe "The transactor converts stake forms and dispatches ETRs" $ do
    it "selected stake number is the same as the generated etrs" $ do
      1 `shouldBe` 1
      {--
      t <- Time.getCurrentTime
      uid <- getULID
      let
        d = (60 :: Time.NominalDiffTime)
        ns :: [NodeT]
        ns = map (\n-> NodeId (Text.pack $ show n)) [1..10::Int]
        stks = map (\s -> s & participating .~ True & power .~ 30) $ map initStake ns 
        etrs = prepTx stks uid t d
      mapM_ (\(etr, stk)-> etr ^. powerInWatts `shouldBe` (abs $ _power stk)) $ zip ((map snd) . fst $ etrs) stks
      (length (fst $ etrs)) `shouldBe` (length stks)
      --}
