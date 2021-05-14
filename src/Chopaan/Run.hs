{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs, FlexibleInstances #-}
module Chopaan.Run (run) where

import GHC.Generics
import Chopaan.Types
import RIO hiding (view, async, withAsync, Async)

import Kbtz
import Chopaan.Utils.Retry
import Chopaan.Kibbutz
import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.NodeId
import Streamly


data ChopaanState = ChopaanState
  { mqttClient :: Bool
  , serverHandle :: Bool
  } deriving (Eq, Ord, Show, Generic)

data KbtzType = Monitor | Ui

run :: RIO App ()
run = do
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
    kibbutzim = ["g"]
    kbtzOpts =
      fmap (\n -> mkKbtzConf (KbtzId n) [] (Left $ mqttOpts{connId=n}) janusHost janusPort) kibbutzim 
  liftIO $ mapM_ runKibbutz kbtzOpts
  --liftIO $ print ("Running Monitor...")
  --liftIO $ mon id (defGrid nodes) sensors runtime
  where
    janusHost = "localhost"
    janusPort = 8182

instance KbtzM (IO) NodeMAC
