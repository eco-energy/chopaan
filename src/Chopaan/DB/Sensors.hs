module Chopaan.DB.Sensors where


import Database.PostgreSQL.Simple (Connection)

import Lens.Micro
import Control.Monad (void)
import Control.Monad.IO.Class



import Proto.NodeMessageSchema.NodeMessages (EnergyState)
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as F
import Chopaan.Node.NodeId (NodeMAC, unNodeId)
import Chopaan.Utils.Time (utcTimeNow)


{--
sensorsTable :: Table Ins Outs
sensorsTable = Table "sensors" (p10 ( required "node_id"
                                    , required "node_time"
                                    , required "battery_voltage"
                                    , required "grid_voltage"
                                    , required "solar_voltage"
                                    , required "load_battery_current"
                                    , required "grid_battery_current"
                                    , required "grid_relay_current"
                                    , required "generation_current"
                                    , required "temperature"
                                   ))



insertEnergyState :: Connection -> NodeMAC -> EnergyState -> IO ()
insertEnergyState conn n es =
  void $ runInsert_ conn ins
  where
    fromES :: Ins
    fromES = ( 1
             , sqlUTCTime . utcTimeNow  $ es ^. F.cpuTime
             , toFields $ es ^. F.batteryVoltage
             , toFields $ es ^. F.gridVoltage
             , toFields $ es ^. F.solarVoltage
             , toFields $ es ^. F.batteryToLoadCurrent
             , toFields $ es ^. F.batteryToGridCurrent
             , toFields $ es ^. F.gridCurrent
             , toFields $ es ^. F.solarInputCurrent
             , toFields $ es ^. F.temperature
             )
    ins = Insert { iTable = sensorsTable
                 , iRows = [fromES]
                 
                 }
--}
