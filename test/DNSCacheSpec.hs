{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module DNSCacheSpec where

import Control.Concurrent.Async (concurrently)
import Network.DNS hiding (lookup)
import Network.DNS.Cache
import Prelude hiding (lookup)
import Data.Maybe
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import Streamly.Binary
import Test.Hspec

cacheConf :: DNSCacheConf
cacheConf = DNSCacheConf {
    resolvConfs    = [
         defaultResolvConf { resolvInfo = RCHostName "8.8.8.8" }
       , defaultResolvConf { resolvInfo = RCHostName "8.8.4.4" }
       ]
  , maxConcurrency = 10
  , minTTL         = 60
  , maxTTL         = 300
  , negativeTTL    = 300
  }

spec :: Spec
spec = describe "withDnsCache" $ do
    it "resolves domains and caches addresses" $ withDNSCache cacheConf $ \cache -> do
        let dom = "www.example.com"
            addr = 584628317
        resolve      cache dom `shouldReturn` Right (Resolved addr)
        resolveCache cache dom `shouldReturn` Just (Right (Hit addr))
        lookupCache  cache dom `shouldReturn` Just addr
        lookup       cache dom `shouldReturn` Just addr
    it "resolves domains and caches nagative" $ withDNSCache cacheConf $ \cache -> do
        let dom = "not-exist.com"
            err = ServerFailure
        resolve      cache dom `shouldReturn` Left err
        resolveCache cache dom `shouldReturn` Just (Left err)
        lookupCache  cache dom `shouldReturn` Nothing
        lookup       cache dom `shouldReturn` Nothing
    it "resolves domains and caches nagative" $ withDNSCache cacheConf $ \cache -> do
        let dom = "non-exist.org"
            err = NameError
        resolve      cache dom `shouldReturn` Left err
        resolveCache cache dom `shouldReturn` Just (Left err)
        lookupCache  cache dom `shouldReturn` Nothing
        lookup       cache dom `shouldReturn` Nothing
    it "waits for another query for the same domain" $ withDNSCache cacheConf $ \cache -> do
        let dom = "www.example.com"
            addr = 584628317
        (res1,res2) <- concurrently (resolve cache dom) (resolve cache dom)
        [res1,res2] `shouldMatchList` [Right (Resolved addr),Right (Hit addr)]
    it "just returns IP address" $ withDNSCache cacheConf $ \cache -> do
        let dom = "192.0.2.1"
            addr = 16908480
        resolve      cache dom `shouldReturn` Right (Numeric addr)
    it "resolves aws requests conncurrently" $ withDNSCache cacheConf $ \cache -> do
      let
        aws = ("https://s3.ap-southeast-1.amazonaws.com/")
        x = S.replicate 1000 aws
        counter = FL.foldl' (\(!j, !n) a -> case a of
                                Nothing -> (j, n + 1)
                                Just _ -> (j + 1, n)
                               ) (0, 0)
      c <- S.fold counter $ S.fromSerial $ S.mapM (lookup cache) x
      c `shouldBe` (1000, 0)

-- files = [
--   "data/hydration/test1/7c:9e:bd:f5:ec:74/fetch/1630/paths/s3paths",
--   "data/hydration/test1/7c:9e:bd:f5:ec:74/fetch/1631/paths/s3paths",
--   "data/hydration/test1/7c:9e:bd:f6:48:88/fetch/1630/paths/s3paths",
--   "data/hydration/test1/7c:9e:bd:f6:48:88/fetch/1631/paths/s3paths",
--   "data/hydration/test1/8c:aa:b5:95:97:c8/fetch/1630/paths/s3paths",
--   "data/hydration/test1/8c:aa:b5:95:97:c8/fetch/1631/paths/s3paths",
--   "data/hydration/test1/8c:aa:b5:97:69:48/fetch/1630/paths/s3paths",
--   "data/hydration/test1/8c:aa:b5:97:69:48/fetch/1631/paths/s3paths",
--   "data/hydration/test1/ac:67:b2:11:f0:28/fetch/1630/paths/s3paths",
--   "data/hydration/test1/ac:67:b2:11:f0:28/fetch/1631/paths/s3paths",
--   "data/hydration/test1/ac:67:b2:11:f2:30/fetch/1630/paths/s3paths",
--   "data/hydration/test1/ac:67:b2:11:f2:30/fetch/1631/paths/s3paths",
--   "data/hydration/test1/ac:67:b2:1c:ec:d8/fetch/1630/paths/s3paths",
--   "data/hydration/test1/ac:67:b2:1c:ec:d8/fetch/1631/paths/s3paths",
--   "data/hydration/test1/c4:4f:33:67:ea:69/fetch/1630/paths/s3paths",
--   "data/hydration/test1/c4:4f:33:67:ea:69/fetch/1631/paths/s3paths"
--   ]
