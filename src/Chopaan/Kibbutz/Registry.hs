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

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.AWS.Things
import Chopaan.Node.NodeId


data Kibbutz = Kibbutz
  { kname :: KbtzName
  , nodes :: [NodeMAC]
  } deriving (Generic)


data KibbutzEvents = StateUpdate deriving (Eq, Ord, Show)

kbtz :: Text.Text -> [NodeMAC] -> Kibbutz
kbtz k = Kibbutz (KbtzId k)

getKibbutz :: Text.Text -> IO Kibbutz
getKibbutz n = do
  ts <- inAwsContext $ getThings n
  let
    ns = map (NodeId . fromJust . thingName) ts
  return $ kbtz n ns
