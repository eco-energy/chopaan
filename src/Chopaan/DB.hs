{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies #-}

module Chopaan.DB (Persisted(..), dbPool, DBPool) where

import Opaleye (
  Field, Table(Table), Insert(..)
  , required, optional, (.==), (.<)
  , arrangeDeleteSql, arrangeInsertManySql
  , arrangeUpdateSql, arrangeInsertManyReturningSql
  , runInsertMany
  , SqlInt4, SqlFloat8, SqlText, SqlTimestamptz, toFields, sqlUTCTime
  )
import Database.PostgreSQL.Simple (Connection, connect, ConnectInfo(..), close)

import Control.Monad
import Control.Monad.IO.Class
import Data.Pool

import Control.Monad.Trans.Reader

import Chopaan.Node.NodeId

import Chopaan.DB.Sensors (insertEnergyState, sensorsTable)
import Chopaan.DB.Nodes (insertNode)
import Chopaan.Types (DBOpts(..))
import Proto.NodeMessageSchema.NodeMessages (EnergyState, HardwareConfig)

import Data.Time
import Data.Maybe
import RIO.Text (unpack)

import Streamly
import qualified Streamly.Prelude as S


type DBPool = Pool Connection

dbPool :: DBOpts -> IO (DBPool)
dbPool dbOpts = createPool (getDbConn dbOpts) close 2 60 10

getDbConn :: forall m. (MonadIO m) => DBOpts -> m Connection
getDbConn DBOpts{..} = liftIO $ connect ConnectInfo
  { connectHost = unpack host
  , connectPort = fromIntegral port
  , connectDatabase = unpack database
  , connectUser = unpack user
  , connectPassword = unpack password
  }

type NodeConn m a = ReaderT (NodeMAC, DBOpts) m a

runInsert :: (MonadIO m) => DBOpts -> NodeMAC -> (Connection -> NodeMAC -> a -> IO ()) -> a -> m ()
runInsert db n i a = (\f -> f a) =<< runReaderT (insertConn i)  (n, db)


fromPool pool = withResource pool insertConn

insertConn :: (MonadIO m) => (Connection -> NodeMAC -> a -> IO ()) -> ReaderT (NodeMAC, DBOpts) m (a -> m ())
insertConn insert = do
  (n, dbOpts) <- ask
  return $ \a -> ((\conn -> liftIO $ insert conn n a)
                      =<< (liftIO $ getDbConn dbOpts))


--instance (IsStream t, MonadIO m) => Persisted t m Transaction where

--insertES :: m (EnergyState)

insertES' :: MonadIO m => DBOpts -> NodeMAC -> EnergyState -> m ()
insertES' dbOpts n = runInsert dbOpts n insertEnergyState

readES :: forall m. (MonadIO m) => DBOpts -> NodeMAC -> m EnergyState
readES db n = (readES' n =<< getDbConn db)
  where
    readES' :: NodeMAC -> Connection -> m (EnergyState)
    readES' = undefined -- selectTable sensorsTable

readN :: (IsStream t, MonadAsync m) => DBOpts -> NodeMAC -> t m HardwareConfig
readN db n = S.repeatM (liftIO (readN' n =<< getDbConn db))
  where
    readN' :: NodeMAC -> Connection -> IO (HardwareConfig)
    readN' = undefined -- selectTable sensorsTable

insertNode' :: MonadIO m => DBOpts -> NodeMAC -> HardwareConfig -> m ()
insertNode' dbOpts n a = liftIO $ runInsert dbOpts n (\conn -> (\_ _ -> runReaderT (insertNode a) (conn, n))) =<< getDbConn dbOpts


type Range = (LocalTime, LocalTime)

class HasPool p where
  type Opts p
  getPool :: (MonadAsync m) => Opts p -> m (Pool p)

instance HasPool Connection where
  type Opts Connection = DBOpts
  getPool = liftIO . dbPool



class (Show n, Show a) => Persisted n a where
  save :: (HasPool p, MonadAsync m) => p -> n -> a -> m (Maybe n)
  retrieve :: (HasPool p, MonadAsync m) => p -> n -> m (Maybe (n, a))

class Key a where
  hasKey :: a -> String

saveStream :: forall p t m n a. (IsStream t, MonadAsync m, Key n, HasPool p, Persisted n a)
  => Pool p
  -> t m (n, a)
  -> m ()
saveStream pool s = withResource pool (S.drain . adapt . serialWrite)
  where
    serialWrite :: p -> t m (Maybe n)
    serialWrite conn = S.mapM (c conn) s
    c :: p -> (n, a) -> m (Maybe n)
    c = \conn (n, a) -> (save conn n a)

{--
readStream :: forall p t m n a.
  (IsStream t, MonadAsync m, Key n, HasPool p, Persisted n a)
  => (Persisted n a)
  => (Pool p) -> (n -> n) -> n -> m (t m (n, a))
readStream pool nextN n r = do
  let asStream con = (fmap fromJust . snd)
        $ S.filter (isJust) readStream
        where
          readStream :: t m (Maybe (n, a))
          readStream = S.iterateM (\prev ->
                        retrieve con (nextN prev))
                       (n, Nothing)
  withResource pool asStream
--}

{--
instance (IsStream t, MonadAsync m) => Persisted t m EnergyState where
  save dbOpts nodeMAC s = S.mapM_ (insertES' dbOpts nodeMAC) $ adapt s
  retrieve = readES 
--}

