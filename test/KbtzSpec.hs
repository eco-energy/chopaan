{-# LANGUAGE TypeApplications, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module KbtzSpec (spec) where

import Chopaan.Kibbutz.Kibbutz
import Chopaan.Node.NodeId
import Streamly
import qualified Streamly.Prelude as S
import Test.Hspec
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Data.Monoid (Sum(..))
import Control.Monad.IO.Class
import qualified Data.Text as Text

instance (KbtzConn t m n, Arbitrary n, Arbitrary a) => Arbitrary (Kbtz t m n a) --where
  --arbitrary = do -- Kbtz <$> (fmap arbitrary)
    --ns <- arbitrary @([n])
    --kbtz ns (\n -> return $ S.repeatM (arbitrary @a)) id

instance Arbitrary NodeMAC where
  arbitrary = (NodeId . Text.pack) <$> arbitrary

trivial = 1 `shouldBe` 1

spec :: Spec
spec = do
  describe "Kbtz can do these things" $ do
    it "Kbtz is a applicative" $
      --verboseBatch (monoid (undefined :: (Kbtz SerialT Gen Int (Double, Double, Double))))
      --verboseBatch (applicative (undefined :: Kbtz SerialT IO (NodeMAC) (Double, Double, Double)))
      trivial
    --it "Kbtz is an applicative" $ trivial
    --it "Kbtz is a Monoid" $ trivial
    --it "Kbtz is a Semigroup" $ trivial
    --it "composed scanning of kbtz with two ends of an isomorphism is identity" $ do
      --(k:[]) <- gens 1 (arbitrary @(Kbtz SerialT IO Int Int))

      --(\k' -> flip scanfn f' k' 0) (scanfn k f 0) `shouldBe` k
      --trivial
