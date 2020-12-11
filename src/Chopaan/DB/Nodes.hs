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

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

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
                               , required "batteryType"
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

type NodeTable = Table Ins Outs


toInsert :: NodeTable -> UTCTime -> NodeMAC -> HardwareConfig -> Insert Ins
toInsert = insut msg
  where
    msg :: UTCTime -> NodeMAC -> HardwareConfig -> Ins
    msg t n hw = ( Nothing
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







insertNode :: (MonadIO m) => HardwareConfig -> ReaderT (Connection, NodeMAC) m ()
insertNode = insert t
  where
    t :: UTCTime -> NodeMAC -> HardwareConfig -> Insert Ins
    t = (toInsert nodeTable)

insut :: (UTCTime -> NodeMAC -> x -> a) -> Table a b -> UTCTime -> NodeMAC -> x -> Insert a
insut toRows table t n x = Insert { iTable = table
                                   , iRows = [toRows t n x]
                                   }


insert :: (MonadIO m) => (UTCTime -> NodeMAC -> x -> Insert a) -> x -> ReaderT (Connection, NodeMAC) m ()
insert ins a = do
  (conn, n) <- ask
  time <- liftIO $ getCurrentTime
  liftIO . void $ runInsert_ conn (ins time n a)


runInsert :: Connection -> NodeMAC -> ReaderT (Connection, NodeMAC) m () -> m ()
runInsert c m d = runReaderT d (c, m) 
