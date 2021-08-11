{-# LANGUAGE TypeApplications, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, StandaloneDeriving, GeneralizedNewtypeDeriving, LambdaCase #-}
module KbtzSpec (spec) where

import Chopaan.Kibbutz
import Chopaan.Kibbutz.Transactor (secondF)
import Chopaan.Kibbutz.KbtzId
import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold.Type as UF
import qualified Streamly.Internal.Data.Fold as FL
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))
import Test.Hspec
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Data.Monoid (Sum(..))
import Control.Monad.IO.Class
import qualified Data.Text as Text
import qualified Data.Map as M

{--
instance (IsStream t, Show n, Ord n, Arbitrary n, Arbitrary a) => Arbitrary (Kbtz t (GenT IO) n a) where
  arbitrary = do -- Kbtz <$> (fmap arbitrary)
    ns <- arbitrary @([n])
    kbtz ns (\n -> return $ S.repeatM (arbitrary @a)) id

instance Arbitrary NodeMAC where
  arbitrary = (NodeId . Text.pack) <$> arbitrary
--}

trivial = 1 `shouldBe` 1

type SI = Sum Int

spec :: Spec
spec = do
  describe "Utilities and such" $ do
    it "turn a fold and a function to an unfold that propagates the fold's accumulator" $ do
      let
        fl :: forall m. (Monad m) => FL.Fold m (Int, (Int, Int)) (Int, M.Map Int Int) 
        fl = secondF $ FL.demux $ M.fromList [(i, FL.sum) | i <- [(0 :: Int)..9]]
        is = S.map (\a -> (1, (mod a 10, a))) $ S.enumerateFromTo 0 99
      s <- S.fold fl is
      --(length s) `shouldBe` (length s')
      s `shouldBe` (1, M.fromList [(0, 0)])
      --trivial




