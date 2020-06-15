module Chopaan.TestClient where

{--

import Proto.NodeMessages ()
import Proto.NodeMessages_Fields

import Data.ProtoLens (defMessage)
import Lens.Micro

import qualified Prelude as P (reverse, head, print)

tqueue <- liftIO (atomically $ initNodeQueue) :: RIO App (NodeQueue NodeT EnergyState)
_ <- liftIO $ forkIO $ forever $ demoTx tqueue

demoTx :: NodeQueue NodeT EnergyState -> IO ()
demoTx oQ = do
  let e = (take 10 es)
  _ <- mapM (uncurry $ writeToNodeQueue oQ) e 
  threadDelay 10000000

es :: [(NodeT, EnergyState)]
es = [((NodeId "24:0a:c4:c6:62:ac" :: NodeT), m i) | i <- [1, 100..]]
m :: Int -> EnergyState
m t = defMessage
      & batteryVoltage .~ 12
      & gridVoltage .~ 60
      & batteryToLoadCurrent .~ 5
      & batteryToGridCurrent .~ 5
      & gridToBatteryCurrent .~ 0
      & solarInputCurrent .~ 10
      & dutyCycle .~ 0
      & cpuTime .~ (fromIntegral $ (1581444138 + t))
--}
