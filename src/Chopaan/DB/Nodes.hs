{-# LANGUAGE TypeApplications, FlexibleContexts #-}
module Chopaan.DB.Nodes where

import           Opaleye (Field, Table(Table),
                          required, optional, (.==), (.<),
                          runInsert_, Insert(..),
                          SqlInt4, SqlFloat8, SqlText, SqlTimestamptz, toFields, sqlUTCTime, rReturning)

import Database.PostgreSQL.Simple (Connection)

import Lens.Micro
import Control.Monad (void)
import Data.Profunctor.Product (p12)

import Proto.NodeMessageSchema.NodeMessages (HardwareConfig)
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as F
import Chopaan.Node.NodeId (NodeMAC, unNodeId)
import Chopaan.Utils.Time (utcTimeNow)

import Data.Time.Clock (UTCTime, getCurrentTime)


type Ins = ( Maybe (Field SqlInt4) -- Id, auto-inserted
           , Field SqlText         -- macaddr
           , Field SqlText         -- batteryType
           , Field SqlFloat8       -- batteryMinV
           , Field SqlFloat8       -- batteryMaxV
           , Field SqlFloat8       -- batteryAmpHours
           , Field SqlFloat8       -- pvOCV
           , Field SqlFloat8       -- pvMPPTv
           , Field SqlFloat8       -- pvMPPTi
           , Field SqlFloat8       -- pvPower
           , Field SqlTimestamptz  -- createdat
           , Field SqlTimestamptz  -- updatedat
           )

type Outs = (Field SqlInt4 -- Id, auto-inserted
           , Field SqlText         -- macaddr
           , Field SqlText         -- batteryType
           , Field SqlFloat8       -- batteryMinV
           , Field SqlFloat8       -- batteryMaxV
           , Field SqlFloat8       -- batteryAmpHours
           , Field SqlFloat8       -- pvOCV
           , Field SqlFloat8       -- pvMPPTv
           , Field SqlFloat8       -- pvMPPTi
           , Field SqlFloat8       -- pvPower
           , Field SqlTimestamptz  -- createdat
           , Field SqlTimestamptz  -- updatedat
           )


nodeTable :: Table Ins Outs
nodeTable = Table "nodes" (p12 ( optional "id"
                               , required "macaddr"
                               , required  "batteryType"
                               , required "batteryMinV"
                               , required "batteryMaxV"
                               , required "batteryAmpHours"
                               , required "pvOCV"
                               , required "pvMPPTv"
                               , required "pvMPPTi"
                               , required "pvPower"
                               , required "createdAt"
                               , required "updatedAt"
                               ))

insertNode :: Connection -> NodeMAC -> HardwareConfig -> IO ([Int])
insertNode conn n hw = do
  time <- getCurrentTime
  runInsert_ conn (ins time)
  where
    ins t = Insert { iTable = nodeTable
                   , iRows = [msg t]
                   , iReturning = rReturning (\(i,_, _, _, _, _, _, _, _, _, _, _) -> i)
                   }
    msg :: UTCTime -> Ins
    msg t = ( Nothing
          , toFields . unNodeId $ n
          , toFields . show $ hw ^. F.battery ^. F.type'
          , toFields . f2d $ hw ^. F.battery ^. F.cutOffVoltage
          , toFields . f2d $ hw ^. F.battery ^. F.maxV
          , toFields . f2d $ hw ^. F.battery ^. F.ampHours
          , toFields . f2d $ hw ^. F.solar ^. F.vOC
          , toFields . f2d $ hw ^. F.solar ^. F.vMPPT
          , toFields . f2d $ hw ^. F.solar ^. F.iMPPT
          , toFields . f2d $ hw ^. F.solar ^. F.ratedPower
          , sqlUTCTime t
          , sqlUTCTime t
          )
    f2d = realToFrac @Float @Double
