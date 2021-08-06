{-# LANGUAGE TypeApplications, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, StandaloneDeriving, GeneralizedNewtypeDeriving, LambdaCase #-}
module KbtzSpec (spec) where

import Chopaan.Kibbutz
import Chopaan.Kibbutz.KbtzId
import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold.Types as UF
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
        fl :: forall m. (Monad m) => FL.Fold m (Int, Int) (M.Map Int Int) 
        fl = FL.demux $ M.fromList [(i, FL.sum) | i <- [(0 :: Int)..9]]
        is = S.map (\a -> (mod a 10, a)) $ S.enumerateFromTo 0 99
      s <- S.fold fl is
      s' <- S.fold FL.mconcat $ S.concatUnfold (foldUF id fl) is
      --(length s) `shouldBe` (length s')
      s' `shouldBe` s 
      --trivial
      

{-# INLINE unFoldrM #-}
unFoldrM :: Applicative m => (a -> m (Maybe (b, a))) -> UF.Unfold m a b
unFoldrM next = UF.Unfold step pure
  where
    {-# INLINE_LATE step #-}
    step st =
        (\case
            Just (x, s) -> Yield x s
            Nothing     -> Stop) <$> next st

-- | Like 'unfoldrM' but uses a pure step function.
--
-- >>> :{
--  f [] = Nothing
--  f (x:xs) = Just (x, xs)
-- :}
--
-- >>> Unfold.fold Fold.toList (Unfold.unfoldr f) [1,2,3]
-- [1,2,3]
--
-- /Since: 0.8.0/
--
{-# INLINE unFoldr #-}
unFoldr :: Applicative m => (a -> Maybe (b, a)) -> UF.Unfold m a b
unFoldr step = unFoldrM (pure . step)

