{-# LANGUAGE PackageImports, TypeApplications, OverloadedStrings, FlexibleContexts, ExplicitForAll, ScopedTypeVariables, TypeApplications, TupleSections, TypeSynonymInstances, FlexibleInstances #-}
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
import qualified Data.Map.Strict as M
import Control.Monad.IO.Class
import qualified Data.Time as Time
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Time.Clock.POSIX
import Chopaan.Hydration.Prefix
import Chopaan.Comm.S3
import Chopaan.Hydrate
import Streamly.Binary
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream.Expand as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Array.Foreign.Type as A
import qualified Streamly.Internal.Data.Array.Stream.Foreign as AS
import Control.Concurrent.STM
import Control.Concurrent.Async

import qualified Streamly.Internal.Data.Time.Units as TU

import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId

spec = parallel $ do
  prefixSpec
  controlSpec
  prefixGenSpec
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
          $ S.map eqTup
          --  $ S.trace (\a -> print $ (a, eqTup a))
          --  $ S.mapM (pure . uncurry (==))
          --  $ S.trace (\(x, y) -> if dbg then print (x, y) else return ())
          $ S.zipWith (,) (S.adapt a) (S.adapt b)
    eqTup = (\(a, b) -> a == b)
type Tup3 a = (a, a, a)


newtype InAMinute = InAMinute (Tup3 Time.UTCTime)
  deriving (Show)

instance Arbitrary InAMinute where
  arbitrary = do
    t0 <- (arbitrary @Time.UTCTime)
    dt <- suchThat (arbitrary @Time.NominalDiffTime) (\x -> (1 <= x) && (x <= 100))
    dt' <- suchThat (arbitrary @Time.NominalDiffTime) (\x -> (1 <= x) && (x <= 100))
    let t1 = Time.addUTCTime dt t0
        t2 = Time.addUTCTime dt' t1
    return $ InAMinute (t0, t1, t2)

instance Arbitrary KbtzName where
  arbitrary = (KbtzId . T.pack) <$> (listOf1 arbitraryPrintableChar)

pipelineSpec :: Spec
pipelineSpec = do
  parallel $ describe "pipeline invariants" $ do
    it "prefix congregation works" $ do
      k <- liftIO $ generate (arbitrary @KbtzName)
      ns <- S.toList $ S.replicateM 10 (liftIO . generate $ (arbitrary @NodeMAC))
      tk <- liftIO . atomically $ mkTKbtz $ M.fromList [(k, ns)]
      (InAMinute (t0, t1, _)) <- liftIO $ generate $ (arbitrary @InAMinute)
      let ufN = unfoldNodes Finite tk
          ps = ufStream (prefixGen Infinite (\_ -> pure True) (Ten2, Second) t0 t1)
          nps = nodePrefixes k (\_ -> pure ()) ufN ps
      r <- S.length $ S.hoist (liftIO)
           $ S.trace (liftIO . print)
           S.|$ S.mapM (uncurry bo)
           --  $ S.trace (liftIO . print)
           $ S.fromWAsync
           $ nps
      r `shouldBe` (6 * (length ns))
      where
        bo :: (HConM m) => NodeMAC -> Prefix -> m ((NodeMAC, Prefix), Int)
        bo n a = return ((n,a), 10)

suc1 = modifyMaxSuccess (const 1)

prefixGenSpec :: Spec
prefixGenSpec = describe "Prefix Generation Invariants for Infinite and finite streams" $ do
  parallel $ describe "should be an appendy monoid" $ do
    --       xs <- liftIO $ arbs @Time.UTCTime 3
    prop "The length of both should be same" $ \(start, now, end) (resP :: Resolution, resF) -> do
      let (x, y, z) = mkFin (dup3 (resP, resF)) (start :: Time.UTCTime, now, end)
      l <- S.length $ S.uniq (x <> y)
      l' <- S.length z
      l `shouldBe` l'
    prop "The appended stream should equal one generated from start to end" $
      \(start, now, end) (resP :: Resolution, resF) -> do
        let (x, y, z) = mkFin (dup3 (resP, resF)) (start, now, end) 
        eqS @S.SerialT @Prefix (S.uniq (x <> y)) z
    where
      dup3 a = (a, a, a)
      mkFin = mkT (Finite, Finite, Finite)
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
controlSpec = parallel $ describe "State Management" $ do
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
  where
    conc = foldl (<>) mempty


prefixSpec :: Spec
prefixSpec = parallel $ describe "Prefix Generation Spec" $ do
  it "length corresponds to time units" $ do
    (length $ prefixRange Second s (Just e)) `shouldBe` 86401
  it "prefix length is sane" $ do
    let
      matchDigs res x = (foldl (\a b -> if b /= x then b else a) x rng) `shouldBe` x
        where
          rng = fmap (digs . unPrefix) $ prefixRange res s (Just e)
    matchDigs Second 10
    matchDigs Ten 9
    matchDigs Ten2 8
    matchDigs Ten3 7
    matchDigs Ten4 6
    matchDigs Ten5 5
    matchDigs Ten6 4
    matchDigs Ten7 3
  where
    digs = ceiling . (logBase 10) . realToFrac
    unM (MilliSecond64 w') = w'
    w = unM worldStart
    s = posixSecondsToUTCTime . fromIntegral $ (div w  1000)
    e = posixSecondsToUTCTime . fromIntegral $ (div w  1000) + 86400

