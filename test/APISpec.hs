{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, TypeApplications, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, OverloadedStrings, FlexibleContexts, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module APISpec (spec) where

-- $ https://docs.servant.dev/en/stable/cookbook/testing/Testing.html

import Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher

import qualified Network.Wai.Handler.Warp         as Warp

import           Servant
import           Servant.Client.Streaming hiding (client)
import Streamly
import qualified Streamly.Prelude as S
import Servant.Streamly

import Chopaan.Graph
import Chopaan.Types (PoolConf(..))
import Chopaan.API.Kbtz
import Chopaan.Server
import Chopaan.Kibbutz.KbtzId

import qualified Data.Time as Ti


import Network.HTTP.Client hiding (Proxy)


spec :: Spec
spec = serverSpec


withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action = do
  p <- (mkDBPools (PoolConf 1 1 1) "localhost" 8182)
  Warp.testWithApplication (pure $ kbtzApp p) action


t0 = Ti.UTCTime (Ti.fromGregorian 2021 4 6) (Ti.secondsToDiffTime 0)
tn = Ti.addUTCTime (60 * 60) t0


serverSpec :: Spec
serverSpec = describe "API TODO" $ do
  let kId = (KbtzId "test")
  around withUserApp $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })
        kbtzClient = client . clientEnv
    describe "Test API" $ do
      it "Can get Kbtzim" $ \p -> do
        let KbtzAPI{..} = kbtzClient p
        a <- _addKbtz kId
        ks <- _getKbtzim
        (head ks) `shouldBe` kId
          

  
