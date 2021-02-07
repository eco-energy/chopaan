{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedStrings, TypeApplications, TypeFamilies #-}
module Chopaan.Kibbutz.AWS.Hydration where

import Lens.Micro

import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Data.Conduit.Combinators (sinkLazy)

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

import Proto.NodeMessageSchema.NodeMessages (MeshFrame)

import Streamly
import Streamly.Prelude as S
import Streamly.Internal.Data.Stream.StreamK (hoist)
import Control.Monad.Trans.Resource

import System.IO

inS3Context :: AWST' Env (ResourceT IO) b -> IO b
inS3Context x = ((newLogger Debug stdout) >>= (\l-> inAwsContext l s3 x))  

toNodeMAC :: S3.ObjectKey -> Maybe (NodeMAC)
toNodeMAC (S3.ObjectKey txt) = (topicToNodeId "/state" txt) 


toMeshframe :: LBS.ByteString -> (Either String MeshFrame)
toMeshframe = decodeMessage . BS.concat . LBS.toChunks


getAddressedMf ::  forall t m. (IsStream t, MonadAsync m)
               => S3.BucketName -> [S3.ObjectKey] -> t m (Maybe NodeMAC, Either String MeshFrame)
getAddressedMf bucket os = S.mapM (\n -> liftIO $
                                  (,)
                                  <$> (pure @IO $ toNodeMAC n)
                                  <*> (pure . toMeshframe =<< readObject bucket n))
                    $ S.fromList os


readObject :: (MonadIO m) => S3.BucketName -> S3.ObjectKey -> m LBS.ByteString
readObject bucket k = liftIO . inS3Context $ do
      x <- send $ S3.getObject bucket k
      (x ^. S3.gorsBody) `sinkBody` sinkLazy

        
listObjects :: forall t m. (IsStream t, MonadAsync m)
              => S3.BucketName
              -> t m (S3.ListObjectsV2Response)
listObjects bucket = hoist (liftIO . inS3Context) $ S.unfold pageUF (S3.listObjectsV2 bucket)


getMeshFramesS3 :: forall t m. (IsStream t, MonadAsync m)
              => S3.BucketName
              -> t m (Maybe NodeMAC, Either String MeshFrame)
getMeshFramesS3 bucket = S.concatMap (getAddressedMf bucket) $
                         S.map (((^. S3.oKey) <$>) . (^. S3.lovrsContents)) $
                         listObjects bucket

bucketN :: S3.BucketName
bucketN = S3.BucketName "dosti-datastream"
