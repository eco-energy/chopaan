module Chopaan.DB.Streams where

import           Opaleye (Field, Table(Table),
                          required, optional, (.==), (.<),
                          runInsert_, Insert(..),
                          SqlInt4, SqlFloat8, SqlText, SqlTimestamptz, toFields, sqlUTCTime)

import Database.PostgreSQL.Simple (Connection)

import Lens.Micro
import Control.Monad (void)
import Data.Profunctor.Product (p9)

import Proto.NodeMessageSchema.NodeMessages (EnergyState)
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as F
import Chopaan.Node.NodeId (NodeMAC, unNodeId)
import Chopaan.Utils.Time (utcTimeNow)



type Ins = ( Field SqlInt4
           , Field SqlText
           , Field SqlTimestamptz
           , Field SqlFloat8
           , Field SqlFloat8
           , Field SqlFloat8
           , Field SqlFloat8
           , Field SqlFloat8
           , Field SqlFloat8)

type Outs = ( Field SqlInt4
            , Field SqlText
            , Field SqlTimestamptz
            , Field SqlFloat8
            , Field SqlFloat8
            , Field SqlFloat8
            , Field SqlFloat8
            , Field SqlFloat8
            , Field SqlFloat8)

streamsTable :: Table Ins Outs
streamsTable = Table "streams" (p9 ( required "node_id"
                                   , required "macaddr"
                                   , required "time"
                                   , required "battery_voltage"
                                   , required "grid_voltage"
                                   , required "load_battery_current"
                                   , required "grid_battery_current"
                                   , required "generation_current"
                                   , required "temperature"
                                   ))



insertEnergyState :: Connection -> NodeMAC -> EnergyState -> IO ()
insertEnergyState conn n es =
  void $ runInsert_ conn ins
  where
    fromES :: Ins
    fromES = ( 1
             , toFields . unNodeId $ n
             , sqlUTCTime . utcTimeNow  $ es ^. F.cpuTime
             , toFields $ es ^. F.batteryVoltage
             , toFields $ es ^. F.gridVoltage
             , toFields $ es ^. F.batteryToLoadCurrent
             , toFields $ es ^. F.batteryToGridCurrent
             , toFields $ es ^. F.solarInputCurrent
             , toFields $ es ^. F.temperature
             )
    ins = Insert { iTable = streamsTable
                 , iRows = [fromES]
                 
                 }
