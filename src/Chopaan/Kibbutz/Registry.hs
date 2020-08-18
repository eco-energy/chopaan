{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Chopaan.Kibbutz.Registry
  ( Kibbutz(..)
  , KibbutzEvents(..)
  , getKibbutz
  ) where

import qualified Data.Text as Text
import GHC.Generics (Generic)
import Data.Maybe (fromJust)

import Chopaan.Kibbutz.AWS.Things
import Chopaan.Node.NodeId

data Kibbutz = Kibbutz
  { kname :: Text.Text
  , nodes :: [NodeMAC]
  } deriving (Generic)


instance Show Kibbutz where
  show Kibbutz {..} = Text.unpack $ (kname <> " Kibbutz, " <> (Text.pack $ show $ length nodes) <> " nodes")

data KibbutzEvents = StateUpdate deriving (Eq, Ord, Show)

kbtz :: Text.Text -> [NodeMAC] -> Kibbutz
kbtz = Kibbutz

getKibbutz :: Text.Text -> IO Kibbutz
getKibbutz n = do
  ts <- getThings n
  let
    ns = map (NodeId . fromJust . thingName) ts
  return $ kbtz n ns



