{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module KMSpec (spec) where

import Node (NodeId(..))
import Registry
import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Streamly.Prelude as S
import Streamly

import qualified Data.Time as Time

import qualified StmContainers.Map as SMap

import Control.Concurrent.STM (atomically)
import Control.Monad (replicateM)
--import Control.Monad.IO.Class (liftIO)

import Test.QuickCheck
import Import (liftIO)

import qualified Data.Text as Text


ns :: Gen NodeT
ns = do
  ns <- mapM (\i -> arbitraryASCIIChar) [1..10]
  return $ NodeId (Text.pack ns)

spec :: Spec
spec = do
  describe "This is how we use node streams" $ do
    let
      --ns' :: m [NodeT]
      --ns' = mapM (\i-> ns) [1..10]
    ns' <- (replicateM 10 $ liftIO $  generate ns)
    initM <- liftIO $ (atomically $ initKibbutzMonitor ns') 
    s <- liftIO $ (atomically $ SMap.size initM)
    it "initializes with all d nodes" $ s `shouldBe` (length ns') 

