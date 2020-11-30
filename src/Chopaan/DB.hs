{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Chopaan.DB where

import Opaleye (
  Field, Table(Table)
  , required, optional, (.==), (.<)
  , arrangeDeleteSql, arrangeInsertManySql
  , arrangeUpdateSql, arrangeInsertManyReturningSql
  , runInsertMany
  , SqlInt4, SqlFloat8, SqlText, SqlTimestamptz, toFields, sqlUTCTime
  )
import Database.PostgreSQL.Simple (Connection, connect, ConnectInfo(..))
import Control.Arrow (returnA)
import Data.Time (Day)

import Chopaan.DB.Sensors (insertEnergyState)
import Chopaan.Types (DBOpts(..), defDBOpts)

import RIO.Text (unpack)

getDbConn :: DBOpts -> IO Connection
getDbConn DBOpts{..} = connect ConnectInfo
  { connectHost = unpack host
  , connectPort = port
  , connectDatabase = unpack database
  , connectUser = unpack user
  , connectPassword = unpack password
  }

