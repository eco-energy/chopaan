{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedStrings, TypeApplications, TypeFamilies #-}
module Chopaan.Comm.S3 where

import Lens.Micro

import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Arrow
import Data.Conduit.Combinators (sinkLazy)

import qualified Data.Text as T
import Data.Maybe
import Data.Either
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

import Proto.NodeMessageSchema.NodeMessages (MeshFrame, EnergyState, RuntimeStats)


import Streamly.Prelude as S
import Streamly.Internal.Data.Stream.StreamK (hoist)
import Control.Monad.Trans.Resource

import System.IO

import Chopaan.Comm.Address
import Chopaan.Comm.Dispatch
import Chopaan.Kibbutz.Kibbutz

inS3Context :: AWST' Env (ResourceT IO) b -> IO b
inS3Context x = ((newLogger Error stdout) >>= (\l-> inAwsContext l s3 x))  

toNodeMAC :: S3.ObjectKey -> Maybe (NodeMAC)
toNodeMAC (S3.ObjectKey txt) = (topicToNodeId "/state/") . cleanMAC
                                $ txt
cleanMAC :: T.Text -> T.Text
cleanMAC = (fst . T.breakOnEnd ("/"))
                               . (T.replace " " "")

toMeshframe :: LBS.ByteString -> (Either String MeshFrame)
toMeshframe = decodeMessage . BS.concat . LBS.toChunks


downloadFromKey :: (MonadAsync m) => S3.BucketName
               -> S3.ObjectKey
               -> m (Maybe NodeMAC, Either String MeshFrame)
downloadFromKey bucket n = do 
                mf <- (pure . toMeshframe =<< readObject bucket n)
                return $ (toNodeMAC n, mf)

readObject :: (MonadIO m) => S3.BucketName -> S3.ObjectKey -> m LBS.ByteString
readObject bucket k = liftIO . inS3Context $ do
      x <- send $ S3.getObject bucket k
      (x ^. S3.gorsBody) `sinkBody` sinkLazy

        
listObjects :: forall t m. (IsStream t, MonadAsync m)
              => S3.BucketName
              -> Maybe T.Text
              -> t m (S3.ListObjectsV2Response)
listObjects bucket prefix = hoist (liftIO . inS3Context) $
                     S.unfold pageUF $ S3.listObjectsV2 bucket
                                     & S3.lovPrefix
                                     .~ prefix

bucketN :: S3.BucketName
bucketN = S3.BucketName "dosti-datastream"

s3Paths :: (IsStream t, MonadAsync m)
        => S3.BucketName
        -> Maybe T.Text
        -> t m (S3.ObjectKey)
s3Paths bucket prefix = S.concatMap (S.fromList)
                        $ fmap (((^. S3.oKey) <$>) . (^. S3.lovrsContents))
                        $ listObjects bucket prefix


s3frames :: forall t m. (IsStream t, MonadAsync m)
         => S3.BucketName
         -> t m (S3.ObjectKey)
         -> t m (Maybe NodeMAC, Either String MeshFrame)
s3frames bucket = S.mapM (downloadFromKey bucket)



s3Prefix :: (Address n) => n -> Maybe T.Text
s3Prefix = Just . stateTopic

nodeS3 :: forall t m. (IsStream t, MonadAsync m) => S3.BucketName
       -> NodeMAC
       -> t m (NodeMAC, Either RuntimeStats EnergyState)
nodeS3 bucket n = process $ s3frames bucket $ s3Paths bucket (s3Prefix n)
  where
    process :: ()
      => t m (Maybe NodeMAC, Either String MeshFrame)
      -> t m (NodeMAC, Either RuntimeStats EnergyState)
    process = S.map ((fromRight undefined))
                . S.filter (isRight)
                . S.map toRL
                . S.filter (isRight . snd)
                . unpackMAC
      where
        unpackMAC :: t m (Maybe a, b)
          -> t m (a, b)
        unpackMAC = S.map (first fromJust)
                . S.filter (isJust . fst)
        toRL :: (NodeMAC, Either String MeshFrame)
          -> Either () (NodeMAC, Either RuntimeStats EnergyState)
        toRL (n, m') = case m' of
            Left _ -> Left ()
            Right m ->
              case accessEnergyState m of
                Just e -> Right $ (n, Right e)
                Nothing ->
                  case accessRTS m of
                    Just r -> Right $ (n, Left r)
                    Nothing -> Left ()

sensorS3 :: (IsStream t, MonadAsync m) => S3.BucketName -> NodeMAC -> t m (EnergyState)
sensorS3 bucket n = S.map ((fromRight undefined) . snd)
                  $ S.filter (isRight . snd)
                  $ S.trace (\x -> liftIO . print $ ("SensorS3: " <> show x))
                  $ nodeS3 bucket n

rsS3 :: (IsStream t, MonadAsync m) => S3.BucketName -> NodeMAC -> t m (RuntimeStats)
rsS3 bucket n = S.map ((fromLeft undefined) . snd)
         $ S.filter (isLeft . snd)
         $ nodeS3 bucket n

