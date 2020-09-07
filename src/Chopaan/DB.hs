{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Chopaan.DB where

import           Opaleye (Field, Table(Table),
                          required, optional, (.==), (.<),
                          arrangeDeleteSql, arrangeInsertManySql,
                          arrangeUpdateSql, arrangeInsertManyReturningSql,
                          runInsertMany,
                          SqlInt4, SqlFloat8, SqlText, SqlTimestamptz, toFields, sqlUTCTime)

import Database.PostgreSQL.Simple (Connection, connect, ConnectInfo(..))

import Control.Arrow (returnA)
import Data.Time (Day)

import Chopaan.DB.Streams (insertEnergyState)



getDbConn :: IO Connection
getDbConn = connect ConnectInfo
  { connectHost = "localhost"
  , connectPort = 5432
  , connectDatabase = "chopaan"
  , connectUser = "chopaan"
  , connectPassword = "opaleye_tutorial"
  }

