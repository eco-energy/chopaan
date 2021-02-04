{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Chopaan.Kibbutz.AWS.Hydration where

import Lens.Micro

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Data.Conduit (sinkLazy)

import Network.AWS.S3 (s3)
import qualified Network.AWS.S3.Types as S3
import qualified Network.AWS.S3.ListObjectsV2 as S3
import qualified Network.AWS.S3.GetObject as S3


import qualified Data.ByteString.Lazy as LBS

import Chopaan.Kibbutz.AWS.Common
import Proto.NodeMessageSchema.NodeMessages (MeshFrame)

import Streamly
import Streamly.Prelude as S


getMeshframes :: forall t m r. (IsStream t, MonadAsync m, AWSConstraint r m)
              => S3.BucketName
              -> t m (Maybe MeshFrame)
getMeshframes bucket = S.mapM (toMeshframe) $
                       S.concatMap (getObjects) $
                       S.map (\x -> fmap (\y -> y ^. S3.oKey) (x ^. S3.lovrsContents)) $
                       S.unfold (pageUF) listObjects
  where
    getObjects :: [S3.ObjectKey] -> t m (RsBody)
    getObjects os = S.mapM getObject $ S.fromList os
    getObject :: S3.ObjectKey -> m RsBody
    getObject k = do
      x <- send $ S3.getObject bucket k
      return $ x ^. S3.gorsBody `sinkBody` sinkLazy
    listObjects = S3.listObjectsV2 bucket


toMeshframe :: RsBody -> m (Maybe MeshFrame)
toMeshframe = undefined
