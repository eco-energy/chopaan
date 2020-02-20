{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module KMSpec (spec) where

import Test.Hspec

import qualified Streamly.Data.Fold as FL
import qualified StmContainers.Map as SMap
import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (liftIO)
import StateMonitor

import Streamly
import qualified Streamly.Prelude as S

spec :: Spec
spec = do
  describe "This is how we use node streams" $ do
    it  "an initialized KM read is equivalent to the (idx, defVal) for idx in indices" $ do
      initM <- liftIO (atomically $ initKM [1..10 :: Int] (0 :: Int))
      s <- liftIO $ (atomically $ SMap.size (runKM $ initM))
      as <- liftIO $ (atomically $ readKM initM [1..10 :: Int])
      (s) `shouldBe` (0)
      (sum $ map snd as) `shouldBe` 0
      
    it "a KM that is written to can be read" $ do
      initM <- liftIO (atomically $ initKM [1..10 :: Int] (0 :: Int))
      liftIO $ S.mapM_ (\(n, s) -> atomically $ ((updateKM initM n s))) $ S.fromList $ zip [1..10 :: Int] [1..10 :: Int]
      uf <- liftIO $ S.length . S.fromList =<< (atomically $ readKM initM [1..10 :: Int])
      uf `shouldBe` (10)
      
    it "an initially serial stream should be able to write concurrently to a KM" $ do
      initM <- liftIO (atomically $ initKM [1..10 :: Int] 1)
      let ss :: (IsStream t, Monad m) => t m (Int, Double)
          ss = S.zipWith (,) (S.enumerateFromTo (1 :: Int) 100) (S.enumerateFromTo (1.0 :: Double) 100)
      S.mapM_ (\(i, s) -> atomically $ updateKM initM i s) ss
      maxEl <- S.fold FL.maximum (S.map snd $ S.fromList =<< (liftIO $ atomically $ readKM initM [1..100 :: Int]))
      maxEl `shouldBe` (Just (100 :: Double))
  {--
    it "mapping updateMonitorState over a KMState and KConnM should result in a KConnM that is the size of the stream" $ do
      let nodes = [NodeId (Text.pack $ replicate 10 a) | a <- ['a'..'z']]
      kmState <- initKMS nodes
      kConnM <- initKMConn nodes
      S.mapM_ (updateMonitorState kmState kConnM) $ S.fromList $ zip nodes [] 
--}
