{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs, FlexibleInstances #-}
module Chopaan.Run (run, mon) where

import GHC.Generics
import Chopaan.Types
import RIO hiding (view, async, withAsync, Async)

import Chopaan.Kibbutz.Kibbutzim

import Chopaan.Server (mon)
import Kbtz
import Chopaan.DB
import Chopaan.Utils.Retry
--import Chopaan.Testing (testNodes, testPub)


data ChopaanState = ChopaanState
  { mqttClient :: Bool
  , serverHandle :: Bool
  } deriving (Eq, Ord, Show, Generic)


--data KbtzSpec k n c = KbtzSpec { kbtzId :: k
--                               , kbtznodes :: [(n, HardwareConfig)]
--                             } deriving (Eq, Ord, Show, Generic)

run :: RIO App ()
run = do
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
    --nodes = testNodes
  --nodes <- (runReaderT getNodes (KbtzId name))
  --kibbutzim = []
  --dbpool <- liftIO . (recoverC 100) $ dbPool dbOpts
  --liftIO . print $ "DB Connection Pool Initialized"
  liftIO . print =<< (liftIO . (recoverC 500) . getSchema $ dbOpts)
  --liftIO $ mapM (runKibbutz @AheadT @IO mqttOpts{connId=name}) kibbutzim
  --liftIO $ print ("Running Monitor...")
  --liftIO $ mon id (defGrid nodes) sensors runtime


