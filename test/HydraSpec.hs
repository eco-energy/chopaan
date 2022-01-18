{-# LANGUAGE PackageImports, TypeApplications, OverloadedStrings, FlexibleContexts, ExplicitForAll, ScopedTypeVariables, TypeApplications, TupleSections, TypeSynonymInstances, FlexibleInstances #-}
module HydraSpec where

import Common
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
import Control.Monad.Trans.Reader
import Control.Monad.Trans
import qualified Data.Time as Time
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Time.Clock.POSIX

import Streamly.Binary
import qualified Streamly.Internal.FileSystem.Event.Linux as Ev
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream.Expand as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Array.Foreign.Type as A
import qualified Streamly.Internal.Data.Array.Stream.Foreign as AS
import qualified Streamly.Internal.Data.Time.Units as TU
import qualified Streamly.Prelude as S
import Streamly.Internal.Data.Time.Units (MilliSecond64(..))

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent
import Path.IO
import Path


import Chopaan.Hydration.Prefix
import Chopaan.Kibbutz.TKbtzim
import Chopaan.Kibbutz.FS
import Chopaan.Node.HW
import Chopaan.Comm.S3
import Chopaan.Hydrate
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId

spec = parallel $ do
  prefixSpec
  controlSpec
  prefixGenSpec
  pipelineSpec
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

newtype PosRange = PosRange (Time.UTCTime, Time.UTCTime)
  deriving Show

instance Arbitrary PosRange where
  arbitrary = do
    t0 <- arbitrary @Time.UTCTime `suchThat` ((> toEnum 0) . Time.utctDay)
    return $ PosRange (t0, Time.addUTCTime (fromInteger (86400 * 182)) t0)


pipelineSpec :: Spec
pipelineSpec = do
  parallel $ describe "pipeline invariants" $ do
    prop "finite prefix generation is complete" $ \fidelity -> do
      (Tag k, ns) <- do
        k <- generate $ arbitrary @(Tag KbtzName) 
        ns <- arbs @(NodeModel) 10
        let ns' = zipWith (\n i -> n {nodeIdx = (HHId i)} ) ns [0, 1..]
        return (k, ns')
      tk <- atomically . mkConfig $ M.singleton k (fromNodeModels ns)
      PosRange (t0, t1) <- liftIO $ generate $ (arbitrary @PosRange)
      let ufN = unfoldNodes Finite tk
          ps = ufStream (prefixGen @S.SerialT Finite (\_ -> pure True) (fidelity, Second) t0 t1)
          nps = S.unfold (nodePrefixes (\_ -> pure ()) ufN ps) k
          expectedYields = (length ns) + (length ns * (ceiling $
                                        (realToFrac $ Time.diffUTCTime t1 t0)
                                        /  (10 ^ (fromEnum fidelity))))
      print $ "expected Yields: " <> show expectedYields
      r <- S.length $ S.fromWAsync nps
      (abs (r - expectedYields) <= 10) `shouldBe` True 

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
        x = S.fromAhead $ prefixGen l0 yes r1 start now ()
        y = S.fromAhead $ prefixGen l1 yes r2 now end ()
        z = S.fromAhead $ prefixGen l2 yes r3 start end ()
        in (x, y, z)

isLastEv :: Either KbtzEv NodeEv -> Bool
isLastEv (Right (UpdateNode _ (HHId 9))) = False
isLastEv _ = True

controlSpec :: Spec
controlSpec = describe "State Management" $ do
  it "Unfolds based off TKbtzim responds to events" $ do
    -- withSystemTempDir "kbtzim" $ \d -> do
    (d :: AbsDir) <- liftIO $ makeAbsolute =<< (parseRelDir "data/test")
    tk <- liftIO . atomically $ emptyTK
    (Tag k, ns) <- do
      k <- generate $ arbitrary @(Tag KbtzName) 
      ns <- arbs @(NodeModel) 10
      let ns' = zipWith (\n i -> n {nodeIdx = (HHId i)} ) ns [0, 1..]
      return (k, ns')
    flip runReaderT d $ do 
      let kModel = (fromNodeModels ns)
          wk = S.mapM_ (onEvT tk) $ S.trace (liftIO . print)
            $ S.takeWhile (isLastEv) watchKbtzim
      S.drain $ (S.fromEffect (createKbtz k kModel)) `S.parallel` (S.fromEffect wk)
      let thisK = toKbtzimHW (M.singleton k kModel)
      thatK <- liftIO . atomically $ getKbtzimHW tk
      lift $ thatK `shouldBe` thisK
      --interpretK (DeleteKbtz (Tag k))
      --deadK <- lift . atomically $ getKbtzimHW tk
      --lift $ deadK `shouldBe` mempty
  where
    oneS = TU.MilliSecond64 10
      --interpretK d (DeleteKbtz (Tag k))
      --deadK <- atomically $ getKbtzimHW tk
      --deadK `shouldBe` mempty
  
  -- it "Adding a kibbutz and its nodes produces the right unfold" $ do
  --   k <- atomically $ newTVar mempty
  --   kadd <- mapM_ (atomically . onEvT k) addKs
  --   newNS' <- S.toList $ S.take (length ns) $ S.unfoldManyRoundRobin (unfoldNodes Infinite k) (S.fromList ks)
  --   (Set.fromList newNS') `shouldBe` (Set.fromList ns)
  --   krm <- mapM_ (atomically . onCommand k) rmKs
  --   noNS <- S.toList $ S.unfoldManyRoundRobin (unfoldNodes Infinite k) (S.fromList ks)
  --   (length noNS) `shouldBe` 0
  -- where
  --   conc = foldl (<>) mempty


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

