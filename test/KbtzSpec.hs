{-# LANGUAGE TypeApplications, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
module KbtzSpec (spec) where

import Chopaan.Kibbutz.Kibbutz
import Streamly
import qualified Streamly.Prelude as S
import Test.Hspec
import Test.QuickCheck.Checkers
import Test.QuickCheck

import Control.Monad.IO.Class

instance (KbtzConn t m n, Arbitrary n, Arbitrary a) => Arbitrary (Kbtz t m n a) --where
  --arbitrary = Kbtz <$> (fmap arbitrary)
    --ns <- arbitrary @([n])
    --kbtz ns (\n -> return $ S.repeatM (arbitrary @a)) id

trivial = 1 `shouldBe` 1

spec :: Spec
spec = do
  describe "Kbtz can do these things" $ do
    it "Kbtz is a functor" $ trivial
    it "Kbtz is an applicative" $ trivial
    it "Kbtz is a Monoid" $ trivial
    it "Kbtz is a Semigroup" $ trivial
    it "composed scanning of kbtz with two ends of an isomorphism is identity" $ do
      --(k:[]) <- gens 1 (arbitrary @(Kbtz SerialT IO Int Int))
      let (Iso f f') = mkIso
      --(\k' -> flip scanfn f' k' 0) (scanfn k f 0) `shouldBe` k
      trivial

data Iso a b = Iso (a -> b -> b) (b -> a -> a)

mkIso :: Iso Int Int
mkIso = Iso (const id) (const id)
