{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, TypeApplications, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module APISpec (spec) where

-- $ https://docs.servant.dev/en/stable/cookbook/testing/Testing.html

import Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher

import qualified Network.Wai.Handler.Warp         as Warp

import           Servant
import           Servant.Client.Streaming
import Streamly
import qualified Streamly.Prelude as S
import Servant.Streamly

import Chopaan.Graph
import Chopaan.API.History
import Chopaan.Server
import Chopaan.Kibbutz.KbtzId

import qualified Data.Time as Ti


import Network.HTTP.Client hiding (Proxy)


spec :: Spec
spec = serverSpec


-- withUserApp :: (Warp.Port -> IO ()) -> IO ()
-- withUserApp action = Warp.testWithApplication (pure $ historyApp "localhost" 8182) action


t0 = Ti.UTCTime (Ti.fromGregorian 2021 4 6) (Ti.secondsToDiffTime 0)
tn = Ti.addUTCTime (60 * 60) t0


serverSpec :: Spec
serverSpec = do
  describe "API TODO" $ do
    it "TODO" $ do
      1 `shouldBe` 1
  -- let kbtzId = (KbtzId "test")

  -- around withUserApp $ do
  --     let getHistory = client (Proxy :: Proxy (HistoryAPI AheadT))
  --     baseUrl <- runIO $ parseBaseUrl "http://localhost"
  --     manager <- runIO $ newManager defaultManagerSettings
  --     let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })
  --     xdescribe "GET Graph" $ do
  --       it "responds with 200" $ \p -> do
  --         withClientM (getHistory kbtzId MeshG t0 tn) (clientEnv p) $
  --           \res -> case res of
  --             Left e -> do
  --               print e
  --             Right r -> do
  --               S.mapM_ print $ adapt r
  --         1 `shouldBe` 1 --(Right (x)) 

  
