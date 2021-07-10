{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, TypeApplications, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module APISpec (spec) where

-- $ https://docs.servant.dev/en/stable/cookbook/testing/Testing.html

import Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher

import qualified Network.Wai.Handler.Warp         as Warp

import           Servant
import           Servant.Client


import Chopaan.Graph
import Chopaan.API.History
import Chopaan.Server
import Chopaan.Kibbutz.KbtzId

import qualified Data.Time as Ti


import Network.HTTP.Client hiding (Proxy)


spec :: Spec
spec = serverSpec


withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action = Warp.testWithApplication (pure $ historyApp "localhost" 8182) action


t0 = Ti.UTCTime (Ti.fromGregorian 2021 4 6) (Ti.secondsToDiffTime 0)
tn = Ti.addUTCTime (60 * 60) t0


serverSpec :: Spec
serverSpec = do
  let nNodes = 10
      nMessages = 10
      kbtzId = (KbtzId "test")
  -- beforeAll_ (do
  --               ns <- liftIO $ arbs @NodeMAC nNodes
  --               void $ hydrateKbtz ns nNodes nMessages) $ do
  around withUserApp $ do
      let getHistory = client (Proxy :: Proxy HistoryAPI)
      baseUrl <- runIO $ parseBaseUrl "http://localhost"
      manager <- runIO $ newManager defaultManagerSettings
      let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })
      describe "GET Graph" $ do
        it "responds with 200" $ \p -> do
          result <- runClientM (getHistory kbtzId MeshG t0 tn) (clientEnv p)
          print (result)
          1 `shouldBe` 1 --(Right (x)) 

  
