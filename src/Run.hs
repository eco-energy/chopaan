{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import UI (runTUI, mkUIChan)
import Mqtt (runMqtt, defMQOpts)
import Registry (getKibbutz, nodes, mkCallback, outQueue, inQueue, queueStream, printQueueStream)
import Node (NodeS, runNodeMonitor)
import Import
import Control.Concurrent (forkIO)
import Streamly
import qualified Streamly.Prelude as S
import qualified Data.Time as Time


run :: RIO App ()
run = do
  kbtz <- liftIO $ getKibbutz thingTypeName
  uiChan <- liftIO $ mkUIChan
  initTime <- liftIO $ Time.getCurrentTime
  _ <- liftIO $ forkIO $ runMqtt defMQOpts (outQueue kbtz) (nodes kbtz) (mkCallback kbtz uiChan)
  -- _ <- liftIO $ forkIO $ printQueueStream . queueStream . inQueue $ kbtz
  -- _ <- liftIO $ (print . show) =<< (readBChan uiChan)
  let
    -- nmNow = runNodeMonitor initTime
    ns' :: Parallel (NodeS)
    ns' = parallely . adapt $ foldr (<>) (runNodeMonitor initTime n s) $ [(runNodeMonitor initTime n' s) | n' <- ns]
      where
        s = queueStream $ inQueue kbtz
        (n:ns) = nodes kbtz
  -- _ <- liftIO $
  liftIO $ runTUI kbtz uiChan
  where
    thingTypeName = "kibbutz-pilot-node"
    updateState = undefined
    toDB = undefined
    
