{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, TupleSections, AllowAmbiguousTypes, BangPatterns #-}

{-# OPTIONS_GHC -ddump-simpl #-}
{-# OPTIONS_GHC -dsuppress-all #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Chopaan.Hydrate ( hydrateKbtz
                       , hydrateKbtz'
                       , hydrateKbtzM
                       , hydrateKbtzM'
                       , Hydration
                       ) where

-- import Control.Arrow
-- import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Concurrent (throwTo, ThreadId)
import Control.Concurrent.Async

import Streamly.Prelude as S (IsStream, MonadAsync, adapt)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Fold as FL
import Chopaan.Utils.Streamly (idFold)
-- import qualified Streamly.Internal.Data.Fold.Type as FL
-- import qualified Streamly.Internal.Data.Fold.Tee as FL
-- import qualified Streamly.Internal.Data.Unfold as UF
-- import qualified Streamly.Internal.Data.Pipe as P
-- import qualified Streamly.Data.Array.Foreign as A

import System.IO (Handle, IOMode(..), openFile, hClose)
import System.Posix.Files
import System.Metrics (newStore)
import System.Remote.Monitoring
import Data.Time
import Data.Either
import Data.ByteString.Char8 (hPutStrLn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
--import Data.Maybe
import qualified Data.Map.Lazy as M

import Network.AWS.S3 (s3, ObjectKey, BucketName(..))

import Chopaan.Types (HydrationOpts(..), toUTC, BufferingOpts(..))
import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, EnergyState)
import Chopaan.Kibbutz.AWS.Common (getAwsEnv, Env)
import Chopaan.Comm.S3
import Chopaan.Node.NodeId
import Chopaan.Node.Folds
import Chopaan.Node.Mesh
import Chopaan.Graph
import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz (KbtzC(..))
import Chopaan.Comm.Monitor

hydrateKbtz' :: forall t . (IsStream t)
             => HydrationOpts
             -> KbtzC NodeMAC
             -> t GraphM Hydration
hydrateKbtz' h = S.concatM . hydrateKbtz h


hydrateKbtzM' :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
             => DBPools
             -> HydrationOpts
             -> KbtzC NodeMAC
             -> t m Hydration
hydrateKbtzM' poo h = S.concatM . (hydrateKbtzM poo h)

hydrateKbtzM :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
             => DBPools
             -> HydrationOpts
             -> KbtzC NodeMAC
             -> m (t m Hydration)
hydrateKbtzM poo h = (pure . S.adapt . S.hoist (runGraphWithDB poo))
                   <=< (runGraphWithDB poo . hydrateKbtz h)

--type Hydration' = (NodeMAC, Either SensorR (MeshNode, RxSignal))

type Hydration = (NodeMAC, Bool)

type Range = (UTCTime, UTCTime)


-- concatIxFoldableWith :: forall t m n a.
--   (Ord n, IsStream t, MonadAsync m) => (forall b. t m b -> t m b -> t m b) -> M.Map n (t m a) -> t m (n, a)
-- concatIxFoldableWith conc = M.foldrWithKey c S.nil
--   where
--     c :: n -> t m a -> t m (n, a) -> t m (n, a)
--     c k v acc = (fmap (k,) v) `conc` acc 
-- {-# INLINE concatIxFoldableWith #-}


concatMapIxFoldableWith :: forall t m n a x.
  (Ord n, IsStream t, MonadAsync m)
  => (forall b. t m b -> t m b -> t m b)
  -> (n -> x -> t m a)
  -> M.Map n x
  -> t m (n, a)
concatMapIxFoldableWith conc f = M.foldrWithKey c S.nil
  where
    c :: n -> x -> t m (n, a) -> t m (n, a)
    c k v acc = (fmap (k,) (f k v)) `conc` acc
    {-# INLINE c #-}
{-# INLINE concatMapIxFoldableWith #-}

cancelServer :: (MonadIO m) => ThreadId -> m ()
cancelServer tid = liftIO $ throwTo tid AsyncCancelled

hydrateKbtz :: forall t. (IsStream t) => HydrationOpts -> KbtzC NodeMAC -> GraphM (t GraphM Hydration)
hydrateKbtz HydrationOpts{dbSave, start, end, s3BucketName, resolution, bufOpts, hPrefix} KbtzC{name, nodes} = do
  store <- liftIO $ newStore
  server <- liftIO $ forkServerWith store "localhost" 8112
  _ <- traverse (cd . fetchDir hPrefix) nodes
  monitors <- mapM (liftIO . nodeMon store . unNodeId) nodes
  env <- getAwsEnv s3
  return $ S.finally (cancelServer (serverThreadId server))
    $ hydrationRate
    $ S.maxBuffer (nodeBuffer bufOpts)
    -- S.|$ S.tapRate 60 (\_ -> hydrationSummary mons)
    $ concatMapIxFoldableWith S.async (nodeS env) (M.fromList (zip nodes monitors))
    where
      prefixes = prefixRange resolution (toUTC start) (toUTC end)
        -- m <- newMon
        -- --gh <- appendHandleFromPath "grid"
        -- --mh <- appendHandleFromPath "mesh"
        -- return $ (gh, mh, m)
        -- where
        --   appendHandleFromPath k = do
        --     e <- fileExist fp
        --     case e of
        --       True -> openFile fp AppendMode
        --       False -> openFile fp WriteMode
        --     where
        --       fp = (nodeSavedFile hPrefix n' k)
      hydrationRate = S.tapRate 10 (liftIO . (print . (prefix <>) . show))
      {-# INLINE hydrationRate #-}
      nodeS :: Env -> NodeMAC -> Monitor -> t GraphM (Bool)
      nodeS env n mon = grider dbSave name n
                   S.|$ mesher dbSave name n
                   S.|$ S.fromAhead
                   $ nodeS3 env buck bufOpts hPrefix mon ps n
        where
          ps = S.trace (liftIO . createPrefixDirs hPrefix n) $ S.fromList prefixes
          -- cleanup :: GraphM Bool
          -- cleanup = do
          --   !gc <- liftIO $ hClose gh
          --   !mc <- liftIO $ hClose mh
          --   return $ True
      {-# INLINE nodeS #-}
      buck = (BucketName s3BucketName)
      
            -- !b <- pE =<< addMon name (n, (x, Nothing))
      --       -- !c <- pE =<< addTx name (n, (x, Nothing, Nothing))
      -- writeToHandle :: Handle -> ObjectKey -> Bool -> IO Bool
      -- writeToHandle h o c = case c of
      --   True -> hPutStrLn h o' >> return True 
      --   False -> return False
      --   where
      --     o' = T.encodeUtf8 (unObject o)
      prefix = "Hydration Rate: "
      pE :: Either SpiderException Bool -> SpiderM Bool
      pE r = case r of
        (Left e) -> (liftIO . print $ e) >> return False
        (Right a) -> return a

pE r = case r of
        (Left e) -> (liftIO . print $ e) >> return False
        (Right a) -> return a

mesher :: forall t. (IsStream t)
  => Bool
  -> KbtzName
  -> NodeMAC
  -> t GraphM (ObjectKey, Either EnergyState RuntimeStats)
  -> t GraphM (ObjectKey, Either EnergyState (MeshNode, RxSignal))
mesher dbSave name n s = S.fromAhead
  $ S.trace (getSaveM)
  $ S.map (mfn)
  $ S.adapt
  $ s
  where
    getSaveM = case dbSave of
      True -> saveM
      False -> pure . (const True)
    mfn :: (ObjectKey, Either x RuntimeStats)
        -> (ObjectKey, Either x (MeshNode, RxSignal)) 
    mfn = fmap (fmap ((meshNodeLink (getGridRoot name))))
    saveM :: (ObjectKey, Either a (MeshNode, RxSignal)) -> GraphM Bool
    saveM (!k, x) = case x of
      (Left _) -> return True
      (Right !r) -> do
              --(liftIO . writeToHandle h k) =<<
        (withSpider $! pE =<< addMeshN (n, r))
{-# INLINE mesher #-}

grider :: forall t. (IsStream t)
  => Bool
  -> KbtzName
  -> NodeMAC
  -> t GraphM (ObjectKey, Either EnergyState (MeshNode, RxSignal))
  -> t GraphM Bool
grider dbSave name n s = S.fromAhead
  $ S.mapM (withSpider . (getSaveG))
  $ S.postscan (FL.unzip idFold sensorFold)
  $ S.map (fmap (fromLeft undefined))
  $ S.filter (isLeft . snd)
  $ S.adapt
  $ s
  where 
    getSaveG = case dbSave of
      True -> saveGrid
      False -> \(_, _) -> pure True
    saveGrid (!k, !x) = pE =<< addFlow name (n, x)
{-# INLINE grider #-}
