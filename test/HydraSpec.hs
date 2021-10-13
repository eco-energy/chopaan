{-# LANGUAGE PackageImports, TypeApplications, OverloadedStrings, FlexibleContexts, ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
module HydraSpec where

import Common
import qualified Streamly.Prelude as S
import Streamly.Internal.Data.Time.Units (MilliSecond64(..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Instances.Time
import Test.QuickCheck.Instances.Text

import Data.List
import Control.Monad.IO.Class
import qualified Data.Time as Time
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Time.Clock.POSIX
import Chopaan.Types (Resolution(..))
import Chopaan.Comm.S3
import Chopaan.Hydrate
import Streamly.Binary
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream.Expand as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import Control.Concurrent.STM
import Control.Concurrent.Async

import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId

spec = do
  prefixSpec
  controlSpec
  --keySpec
  --frameSpec
  --ingestionSpec

eqS :: forall t a. (S.IsStream t, Eq a, Show a) => t IO a -> t IO a -> Expectation
eqS a b = do
  r <- liftIO $ sEq 
  r `shouldBe` (Just True)
  where
    sEq :: IO (Maybe Bool)
    sEq = S.the
          --  $ S.map eqTup
          --  $ S.trace (\a -> print $ (a, eqTup a))
          $ S.zipWith (==) (S.adapt a) (S.adapt b)
    -- eqTup = (\(a, b) -> a == b)
type Tup3 a = (a, a, a)


prefixGenSpec :: Spec
prefixGenSpec = modifyMaxSuccess (const 1000) $ describe "Prefix Generation Invariants for Infinite and finite streams" $ do
  describe "should be an appendy monoid" $ do
    --       xs <- liftIO $ arbs @Time.UTCTime 3
    prop "The length of both should be same" $ \(start, now, end) -> do
      let (x, y, z) = mkFin (start :: Time.UTCTime, now, end)
      l <- S.length $ S.uniq (x <> y)
      l' <- S.length z
      l `shouldBe` l'
    prop "The appended stream should equal one generated from start to end" $
      \(start, now, end) -> do
        let (x, y, z) = mkFin (start, now, end)
        eqS @S.SerialT @Prefix (S.uniq (x <> y)) z
    prop "Infinite Streams should respect the Resolution Difference Switch" $
      \(start, now, end) (resP, resF)-> do
        let (x, y, z) = mkT (Finite, Finite, Infinite) (dup3 (resP, resF)) (start, now, end)
        let r = S.uniq (x <> y)
        l <- S.length r
        let z' = (S.take l z)
        eqS @S.SerialT @Prefix r z' 
    where
      dup3 a = (a, a, a)
      mkFin = mkT (Finite, Finite, Finite) (dup3 (Minute, Minute))
      mkT :: Tup3 LifeTime
          -> Tup3 (Resolution, Resolution)
          -> Tup3 Time.UTCTime
          -> Tup3 (S.Serial Prefix)
      mkT (l0, l1, l2) (r1, r2, r3) (s, n, e) = let
        [start, now, end] = sortBy compare [s, n, e]
        yes _ = return True
        x = prefixGen l0 yes r1 start now ()
        y = prefixGen l1 yes r2 now end ()
        z = prefixGen l2 yes r3 start end ()
        in (x, y, z)
    
controlSpec :: Spec
controlSpec = describe "State Management" $ do
  let ks = (KbtzId . T.pack . (pure @[])) <$> ['a'..'d']
      ns = (NodeId . T.pack . show) <$> [1..12]
      kns = fst $ foldr zop ([], ns) ks
        where
          zop :: k -> ([(k, [n])], [n]) -> ([(k, [n])], [n]) 
          zop k (k', n') = ((k, take 3 n') : k', drop 3 n')
      addKs = (uncurry StartKbtz) <$> kns
      rmKs = StopKbtz <$> ks
      addNs = conc $ (\(k', ns') -> (StartNode k' <$> ns')) <$> kns
      rmNs = conc $ (\(k', ns') -> (StopNode k' <$> ns')) <$> kns
  it "Adding a kibbutz and its nodes produces the right unfold" $ do
    k <- atomically $ newTVar mempty
    kadd <- mapM_ (atomically . onCommand k) addKs
    newNS' <- S.toList $ S.take (length ns) $ S.unfoldManyRoundRobin (unfoldNodes Infinite k) (S.fromList ks)
    (Set.fromList newNS') `shouldBe` (Set.fromList ns)
    krm <- mapM_ (atomically . onCommand k) rmKs
    noNS <- S.toList $ S.unfoldManyRoundRobin (unfoldNodes Infinite k) (S.fromList ks)
    (length noNS) `shouldBe` 0
    -- mapM_ (atomically . onCommand k) ((flip StartKbtz []) <$> ks)
    -- nadd <- mapM_ (atomically . onCommand k) addNs
    -- nrm <- mapM_ (atomically . onCommand k) rmNs
    -- mapM_ (atomically . onCommand k) rmKs
    -- 1 `shouldBe` 1
  where
    conc = foldl (<>) mempty


prefixSpec :: Spec
prefixSpec = describe "Prefix Generation Spec" $ do
  it "length corresponds to time units" $ do
    (length $ prefixRange Second s e) `shouldBe` 8641
  it "prefix length is sane" $ do
    let
      matchDigs res x = (foldl (\a b -> if b /= x then b else a) x rng) `shouldBe` x
        where
          rng = fmap (digs . unPrefix) $ prefixRange res s e
    matchDigs Second 9
    matchDigs Minute 7
    matchDigs Hour 5
    matchDigs Day 4
    matchDigs Week 3
  where
    digs = ceiling . (logBase 10) . realToFrac
    unM (MilliSecond64 w') = w'
    w = unM worldStart
    s = posixSecondsToUTCTime . fromIntegral $ (div w  1000)
    e = posixSecondsToUTCTime . fromIntegral $ (div w  1000) + 86400

