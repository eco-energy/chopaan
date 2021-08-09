{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedStrings, TypeApplications, TypeFamilies, DeriveGeneric, StandaloneDeriving, DeriveAnyClass, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Chopaan.Comm.S3 where

import Lens.Micro

import GHC.Generics
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.AWS
import Control.Monad.Catch
import Control.Arrow
import Data.Conduit.Combinators (sinkLazy)

import Data.List (sort)
import qualified Data.Text as T
import Data.Maybe
import Data.Either
import Data.Void
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as TP
import Network.AWS.S3 (s3)
import qualified Network.AWS.S3.Types as S3
import qualified Network.AWS.S3.ListObjectsV2 as S3
import qualified Network.AWS.S3.GetObject as S3

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.ProtoLens.Encoding (decodeMessage)

import Chopaan.Kibbutz.AWS.Common
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.AWS.Things
import Chopaan.Utils.Time

import Proto.NodeMessageSchema.NodeMessages (MeshFrame, EnergyState, RuntimeStats)


import qualified Streamly.Prelude as S
import Streamly.Prelude (IsStream, MonadAsync, adapt)
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream.IsStream  as S
import Control.Monad.Trans.Resource

import System.IO

import Chopaan.Comm.Address
import Chopaan.Comm.Dispatch
import Chopaan.Kibbutz.Kibbutz


toNodeMAC :: S3.ObjectKey -> Maybe (NodeMAC, Time.UTCTime)
toNodeMAC (S3.ObjectKey txt) = do
  (nodePath, filename) <- cleanMAC txt
  nodeId <- topicToNodeId "/state/" nodePath
  ts <- Just . parseUTCTime $ filename
  return (nodeId, ts)

cleanMAC :: T.Text -> Maybe (T.Text, T.Text)
cleanMAC = Just . (T.breakOnEnd ("/")) . (T.replace " " "")

downloadMF :: forall m. (MonadIO m, MonadCatch m)
                => Env
                -> S3.BucketName
                -> S3.ObjectKey
                -> m ((Maybe NodeMAC, Maybe Time.UTCTime), Either String MeshFrame)
downloadMF env bucket n = do
                mf <- liftIO $ withAwsEnv env (readObject bucket n)
                return $ ((nodeMAC, nodeTime), decodeMessage mf)
  where
    nt = toNodeMAC n
    nodeMAC = fmap fst nt
    nodeTime = fmap snd nt

readObject :: forall m. (MonadIO m, MonadCatch m)
           => S3.BucketName -> S3.ObjectKey -> AWST' Env (ResourceT m) BS.ByteString
readObject bucket k = do
      x <- send $ S3.getObject bucket k
      BS.concat . LBS.toChunks <$> (x ^. S3.gorsBody) `sinkBody` sinkLazy


s3Paths :: forall m. (MonadIO m, MonadCatch m)
        => UF.Unfold (AWST' Env (ResourceT m)) S3.ListObjectsV2 (S3.ObjectKey)
s3Paths = let
  plist = UF.map (((^. S3.oKey) <$>) . (^. S3.lovrsContents)) pageUF
  in UF.many plist UF.fromList


s3Prefix :: (Address n) => n -> Maybe T.Text
s3Prefix = Just . stateTopic


nodeS3 :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
       => S3.BucketName
       -> NodeMAC
       -> Maybe S3.ObjectKey
       -> t m (S3.ObjectKey, ((NodeMAC, Maybe Time.UTCTime), Either EnergyState RuntimeStats))
nodeS3 bucket n startAfter = S.concatM $ do
  env <- getAwsEnv s3
  let paths = adapt $ S.hoist (liftIO . withAwsEnv env) $ S.unfold s3Paths req
      mfs = S.mapM (\p -> ((\x -> return (p, x)) =<< (downloadMF env bucket p))) paths
  return $ process mfs
  where
    unObject (S3.ObjectKey k) = k
    req = S3.listObjectsV2 bucket
          & S3.lovPrefix .~ (s3Prefix n)
          & S3.lovStartAfter .~ (fmap unObject startAfter)
    process :: forall n x. (MonadAsync n) => t n (x, ((Maybe NodeMAC, Maybe Time.UTCTime), Either String MeshFrame))
           -> t n (x, ((NodeMAC, Maybe Time.UTCTime), Either EnergyState RuntimeStats))
    process = S.mapM (pure . (second . second $ throwMFError))
              . S.filter (isRight . snd . snd)
              . unpackMAC
      where
        unpackMAC :: t n (x, ((Maybe a, Maybe c), b))
                  -> t n (x, ((a, Maybe c), b))
        unpackMAC = S.mapM (pure . (second . first . first $ fromJust))
                    . S.filter (isJust . fst . fst . snd)
        throwMFError :: Either String MeshFrame -> (Either EnergyState RuntimeStats)
        throwMFError m' = case m' of
          Left e -> error $ "nodeS3 ::" <> (show n) <> "Parse Meshframe Failed: " <> e 
          Right m ->
            case accessEnergyState m of
              Just e -> Left e
              Nothing ->
                case accessRTS m of
                  Just r -> Right r
                  Nothing -> error $ ""


metadataKey :: S3.ObjectKey
metadataKey = "chopaanMetadata"

bucketN :: S3.BucketName
bucketN = S3.BucketName "dosti-datastream"


-- createBucketIndex :: IO ()
-- createBucketIndex = do
--   l <- liftIO $ newLogger Info stdout
--   let xs = (\x -> (toNodeMAC x, x))
--         <$> (s3Paths @S.ParallelT l bucketN (Just "/kibbutz/") Nothing)
--   S.fold 
--   return ()
-- -- What would the structure of the metadata be?
-- -- Is there a greskell expression to fetch all nodes and edges in a graph?
-- fetchMetadata = undefined

-- writeMetadata = undefined

-- sensorS3 :: (IsStream t, MonadAsync m) =>  Logger -> S3.BucketName -> NodeMAC -> t m (EnergyState)
-- sensorS3 l bucket n = S.map ((fromLeft undefined) . snd)
--                   S.|$ S.filter (isLeft . snd)
--                   S.|$ nodeS3 l bucket n Nothing

-- rsS3 :: (IsStream t, MonadAsync m) => Logger -> S3.BucketName -> NodeMAC -> t m (RuntimeStats)
-- rsS3 l bucket n = S.map ((fromRight undefined) . snd)
--          $ S.filter (isRight . snd)
--          $ nodeS3 l bucket n Nothing

