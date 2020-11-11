{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.Run (run, mon) where

import Chopaan.Types
import RIO hiding (view)
import Control.Concurrent (forkIO)

import Streamly
import qualified Streamly.Prelude as S
import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.Comm (MessageQs(..), initQs, mkCallback)

import Chopaan.Kibbutz.Kibbutz (sensorKbtz, rsKbtz, getNodes, asMapStream)
import Chopaan.Kibbutz.Transactor (runTransactor)

import Chopaan.UI (mon)


import           Shpadoinkle                 (Html, JSM)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html
import           Shpadoinkle.Run             (live, runJSorWarp, simple)


view :: () -> Html m ()
view _ = "hello world"

app :: JSM ()
app = simple runParDiff () view getBody


devUI :: IO ()
devUI = live 8080 app


mainUI :: IO ()
mainUI = do
  putStrLn "\nHappy point of view on https://localhost:8080\n"
  runJSorWarp 8080 app




run :: RIO App ()
run = do
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  qs@MessageQs{..} <- liftIO . atomically $ initQs
  nodes <- runReaderT getNodes name
  _ <- liftIO $ forkIO $ forever $
       runMqtt mqttOpts outbox nodes (mkCallback qs)
  let 
    sensors = sensorKbtz @SerialT nodes stateQ
    runtime = rsKbtz @SerialT @IO nodes statsQ
  (txMonitor, txs) <- liftIO $ runTransactor outbox (60*5) sensors
  --return ()
  liftIO $ mainUI
  --liftIO $ forkIO $ S.drain $ S.trace (writeToDB DBConf) sensors
  --liftIO $ mainWidget $ mon id sensors runtime logs txs txMonitor




data DBConf = DBConf

writeToDB :: DBConf -> a -> IO ()
writeToDB = undefined
{--
  
--}
{--liftIO $ printer $ asMapStream sensors
  liftIO $ printer $ asMapStream runtime
  liftIO $ printer $ asMapStream logs
  liftIO $ printer txMonitor
  liftIO $ S.mapM_ print txs--}
