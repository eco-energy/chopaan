{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies #-}

module Chopaan.DB (Persisted(..), dbPool, DBPool, getSchema) where


import Control.Monad.IO.Class
import Data.Text (unpack)

import Streamly
import qualified Streamly.Prelude as S

import Database.Beam.Migrate.Simple
import Database.Beam.Postgres (runBeamPostgres)
import Database.Beam.Postgres.Migrate (migrationBackend)
import Database.PostgreSQL.Simple (Connection, connect, ConnectInfo(..), close)

import Data.Pool
import Chopaan.Types (DBOpts(..))

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


getSchema :: forall m. (MonadIO m) => DBOpts -> m String
getSchema opts = do
  pg <- getDbConn opts
  liftIO $ print $ "got DB Conn"
  liftIO $ runBeamPostgres pg (haskellSchema migrationBackend)

class HasPool p where
  type Opts p
  getPool :: (MonadAsync m) => Opts p -> m (Pool p)

instance HasPool Connection where
  type Opts Connection = DBOpts
  getPool = liftIO . dbPool



class (Show n, Show a) => Persisted n a where
  save :: (HasPool p, MonadAsync m) => p -> n -> a -> m (Maybe n)
  retrieve :: (HasPool p, MonadAsync m) => p -> n -> m [(n, a)]


saveStream :: forall p t m n a. (IsStream t, MonadAsync m, HasPool p, Persisted n a)
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

