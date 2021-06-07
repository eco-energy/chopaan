{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedStrings, TypeApplications, TypeFamilies #-}
module Chopaan.Comm.S3 where

import Lens.Micro

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Arrow
import Data.Conduit.Combinators (sinkLazy)

import Data.List (sort)
import qualified Data.Text as T
import Data.Maybe
import Data.Either
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
import Streamly.Internal.Data.Stream.StreamK (hoist)
import Control.Monad.Trans.Resource

import System.IO

import Chopaan.Comm.Address
import Chopaan.Comm.Dispatch
import Chopaan.Kibbutz.Kibbutz

inS3Context :: Logger -> AWST' Env (ResourceT IO) b -> IO b
inS3Context l x = inAwsContext l s3 x  

toNodeMAC :: S3.ObjectKey -> Maybe (NodeMAC, Time.UTCTime)
toNodeMAC (S3.ObjectKey txt) = do
  (nodePath, filename) <- cleanMAC txt
  nodeId <- topicToNodeId "/state/" nodePath
  ts <- Just . parseUTCTime $ filename 
  return (nodeId, ts)

cleanMAC :: T.Text -> Maybe (T.Text, T.Text)
cleanMAC = Just . (T.breakOnEnd ("/")) . (T.replace " " "")

toMeshframe :: BS.ByteString -> (Either String MeshFrame)
toMeshframe = decodeMessage


downloadFromKey :: (MonadAsync m) => Logger
               -> S3.BucketName
               -> S3.ObjectKey
               -> m ((Maybe NodeMAC, Maybe Time.UTCTime), Either String MeshFrame)
downloadFromKey l bucket n = do 
                mf <- (pure . toMeshframe =<< readObject l bucket n)
                return $ ((nodeMAC, nodeTime), mf)
  where
    nt = toNodeMAC n
    nodeMAC = fmap fst nt
    nodeTime = fmap snd nt

readObject :: (MonadIO m) => Logger -> S3.BucketName -> S3.ObjectKey -> m BS.ByteString
readObject l bucket k = liftIO . (inS3Context l) $ do
      x <- send $ S3.getObject bucket k
      BS.concat . LBS.toChunks <$> (x ^. S3.gorsBody) `sinkBody` sinkLazy

        
listObjects :: forall t m. (IsStream t, MonadAsync m)
              => Logger
              -> S3.BucketName
              -> Maybe T.Text
              -> t m (S3.ListObjectsV2Response)
listObjects l bucket prefix = hoist (liftIO . inS3Context l) $
                     S.asyncly $ S.unfold pageUF $ S3.listObjectsV2 bucket
                                     & S3.lovPrefix
                                     .~ prefix

bucketN :: S3.BucketName
bucketN = S3.BucketName "dosti-datastream"


s3Paths :: forall t m. (IsStream t, MonadAsync m)
        => Logger
        -> S3.BucketName
        -> Maybe T.Text
        -> t m (S3.ObjectKey)
s3Paths l bucket prefix =
  S.concatMapWith S.parallel sortConsume
   S.|$ fmap (((^. S3.oKey) <$>) . (^. S3.lovrsContents))
   S.|$ listObjects l bucket prefix
  where
    -- S.|$ S.trace (liftIO . print) 
    comparator = (\a b -> fromMaybe EQ $ liftA2 compare (x a) (x b) )
    sortConsume :: (Ord a) => [a] -> t m a
    sortConsume = (S.fromList . sort)
    x = fmap snd . toNodeMAC

s3frames :: forall t m. (IsStream t, MonadAsync m)
         => Logger
         -> S3.BucketName
         -> t m (S3.ObjectKey)
         -> t m ((Maybe NodeMAC, Maybe Time.UTCTime), Either String MeshFrame)
s3frames l bucket = S.mapM (downloadFromKey l bucket)



s3Prefix :: (Address n) => n -> Maybe T.Text
s3Prefix = Just . stateTopic


process :: forall t m. (IsStream t, MonadAsync m)
      => t m ((Maybe NodeMAC, Maybe Time.UTCTime), Either String MeshFrame)
      -> t m ((NodeMAC, Maybe Time.UTCTime), Either EnergyState RuntimeStats)
process = S.map ((fromRight undefined))
          . S.filter (isRight)
          . S.map toRL
          . S.filter (isRight . snd)
          . unpackMAC
  where
    unpackMAC :: t m ((Maybe a, Maybe c), b)
              -> t m ((a, Maybe c), b)
    unpackMAC = S.map (first (first fromJust))
      . S.filter (isJust . fst . fst)
    toRL :: ((NodeMAC, Maybe Time.UTCTime), Either String MeshFrame)
         -> Either () ((NodeMAC, Maybe Time.UTCTime), Either EnergyState RuntimeStats)
    toRL ((n, t), m') = case m' of
      Left _ -> Left ()
      Right m ->
        case accessEnergyState m of
          Just e -> Right $ ((n, t), Left e)
          Nothing ->
            case accessRTS m of
              Just r -> Right $ ((n, t), Right r)
              Nothing -> Left ()

nodeS3 :: forall t m. (IsStream t, MonadAsync m) => Logger
       -> S3.BucketName
       -> NodeMAC
       -> t m ((NodeMAC, Maybe Time.UTCTime), Either EnergyState RuntimeStats)
nodeS3 l bucket n = process S.|$ (s3frames l) bucket
                  -- $ S.trace (liftIO . print)
                  $ s3Paths l bucket $ s3Prefix n
    

sensorS3 :: (IsStream t, MonadAsync m) =>  Logger -> S3.BucketName -> NodeMAC -> t m (EnergyState)
sensorS3 l bucket n = S.map ((fromLeft undefined) . snd)
                  S.|$ S.filter (isLeft . snd)
                  S.|$ nodeS3 l bucket n

rsS3 :: (IsStream t, MonadAsync m) => Logger -> S3.BucketName -> NodeMAC -> t m (RuntimeStats)
rsS3 l bucket n = S.map ((fromRight undefined) . snd)
         $ S.filter (isRight . snd)
         $ nodeS3 l bucket n

