{-# LANGUAGE OverloadedStrings #-}

module Chopaan.Kibbutz.NetSpider where

import Control.Applicative ((<$>), (<*>))
import Control.Category ((<<<))
import Control.Exception (bracket)
import Data.Either (partitionEithers)
import Data.List (sort, sortOn)
import Data.Text (Text)

import qualified Data.Text.Lazy.IO as TLIO
import NetSpider.Pair (Pair(..))
import NetSpider.Spider
  (Spider, connectWS, close, addFoundNode, clearAll, getSnapshotSimple)
import NetSpider.Found
  (FoundNode(..), FoundLink(..), LinkState(LinkBidirectional))
import NetSpider.GraphML.Writer (writeGraphML)
import NetSpider.Timestamp (fromS)
import NetSpider.Snapshot
  (nodeId, nodeTimestamp, linkNodePair, linkTimestamp)

import Chopaan.Kibbutz.Mesh
import Chopaan.Comm.Comm (Address(..))

type SpiderFn n l = (Spider Text n l -> IO ())

withSpider :: SpiderFn n l -> IO ()
withSpider = bracket (connectWS gremlinHost gremlinPort) close
  where
    gremlinHost = ""
    gremlinPort = 8081




