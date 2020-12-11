{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, RankNTypes #-}

module Chopaan.DB (Persisted(..), getDbConn) where

import Opaleye (
  Field, Table(Table), Insert(..)
  , required, optional, (.==), (.<)
  , arrangeDeleteSql, arrangeInsertManySql
  , arrangeUpdateSql, arrangeInsertManyReturningSql
  , runInsertMany
  , SqlInt4, SqlFloat8, SqlText, SqlTimestamptz, toFields, sqlUTCTime
  )
import Database.PostgreSQL.Simple (Connection, connect, ConnectInfo(..))
import Control.Monad.IO.Class

import Control.Monad.Trans.Reader

import Chopaan.Node.NodeId

import Chopaan.DB.Sensors (insertEnergyState, sensorsTable)
import Chopaan.DB.Nodes (insertNode)
import Chopaan.Types (DBOpts(..))
import Proto.NodeMessageSchema.NodeMessages (EnergyState, HardwareConfig)

import RIO.Text (unpack)

import Streamly
import qualified Streamly.Prelude as S


getDbConn :: DBOpts -> IO Connection
getDbConn DBOpts{..} = connect ConnectInfo
  { connectHost = unpack host
  , connectPort = fromIntegral port
  , connectDatabase = unpack database
  , connectUser = unpack user
  , connectPassword = unpack password
  }

type NodeConn m a = ReaderT (NodeMAC, DBOpts) m a

runInsert :: (MonadIO m) => DBOpts -> NodeMAC -> (Connection -> NodeMAC -> a -> IO ()) -> a -> m ()
runInsert db n i a = (\f -> f a) =<< runReaderT (insertConn i)  (n, db)

insertConn :: (MonadIO m) => (Connection -> NodeMAC -> a -> IO ()) -> ReaderT (NodeMAC, DBOpts) m (a -> m ())
insertConn insert = do
  (n, dbOpts) <- ask
  return $ \a -> ((\conn -> liftIO $ insert conn n a)
                      =<< (liftIO $ getDbConn dbOpts))


--instance (IsStream t, MonadIO m) => Persisted t m Transaction where

--insertES :: m (EnergyState)

insertES' :: MonadIO m => DBOpts -> NodeMAC -> EnergyState -> m ()
insertES' dbOpts n = runInsert dbOpts n insertEnergyState

readES :: (IsStream t, MonadAsync m) => DBOpts -> NodeMAC -> t m EnergyState
readES db n = S.repeatM (liftIO (readES' n =<< getDbConn db))
  where
    readES' :: NodeMAC -> Connection -> IO (EnergyState)
    readES' = undefined -- selectTable sensorsTable

readN :: (IsStream t, MonadAsync m) => DBOpts -> NodeMAC -> t m HardwareConfig
readN db n = S.repeatM (liftIO (readN' n =<< getDbConn db))
  where
    readN' :: NodeMAC -> Connection -> IO (HardwareConfig)
    readN' = undefined -- selectTable sensorsTable

insertNode' :: MonadIO m => DBOpts -> NodeMAC -> HardwareConfig -> m ()
insertNode' dbOpts n a = liftIO $ runInsert dbOpts n (\conn -> (\_ _ -> runReaderT (insertNode a) (conn, n))) =<< getDbConn dbOpts



class (IsStream t, MonadAsync m) => Persisted t m a where
  save :: DBOpts -> NodeMAC -> t m a -> m ()
  retrieve :: DBOpts -> NodeMAC -> t m a

instance (IsStream t, MonadAsync m) => Persisted t m EnergyState where
  save dbOpts nodeMAC s = S.mapM_ (insertES' dbOpts nodeMAC) $ adapt s
  retrieve = readES 

instance (IsStream t, MonadAsync m) => Persisted t m HardwareConfig where
  save dbOpts nodeMAC s = S.mapM_ (insertNode' dbOpts nodeMAC) $ adapt s
  retrieve = readN
