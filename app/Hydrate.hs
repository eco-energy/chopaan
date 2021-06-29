{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeApplications, ScopedTypeVariables, ExplicitForAll #-}
module Main where

import Control.Arrow (first, second)
import Control.Monad
import Chopaan.Comm.S3
import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz
import Chopaan.Kibbutz.Kibbutz (getNodes)
import Chopaan.Node.Mesh
import Chopaan.Node.NodeSensors
import Chopaan.Node.NodeId
import Chopaan.Utils.Time
import Chopaan.Comm.Address
import Chopaan.Kibbutz.AWS.Common (newLogger, LogLevel(..), Logger)
import Data.Greskell
import Data.Either
import Data.Bifunctor hiding (second)
import Data.ProtoLens.Encoding
import qualified Data.Text as T

import qualified Network.AWS.S3 as S3

import qualified Streamly.Prelude as S
import Streamly as S --(IsStream, MonadAsync, ParallelT, adapt)
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Array as Ar


import qualified System.IO as I
import qualified Data.ByteString as BS (uncons)
import qualified Data.ByteString.Lazy as BSL (toStrict)
import Data.Binary as B

main :: IO ()
main = do
  tings <- getNodes thingType
  l <- newLogger Info I.stdout
  S.drain $ S.parallely $ S.mapM (downloadNode l) $ S.fromList tings
  --S.parallely (downloadNode @S.AheadT <$>
  where
    thingType = KbtzId "kibbutz-pilot-node"
    janusHost = "localhost"
    janusPort = 8182
    -- (\n ->
    --          S.fold (writePathsFold n)
    --          $ s3Paths bucketN (nodePrefix n))


downloadNode :: Logger -> NodeMAC -> IO () -- forall t. (S.IsStream t) => t IO ()  
downloadNode l n = S.drain $ S.bracket opF cF $ \h -> S.scan (FH.write h)
  S.|$ S.concatMap (S.unfoldr (BS.uncons))
  S.|$ S.map (BSL.toStrict . B.encode . bimap (fst) (either encodeMessage encodeMessage))
  --S.|$ S.trace print
  S.|$ process
  S.|$ S.asyncly $ s3frames l bucketN
  S.|$ S.tap (writePathsFold n)
  --S.|$ S.trace print
  S.|$ S.parallely $ s3Paths l bucketN $ nodePrefix n
  where
    opF = I.openFile fp I.WriteMode
    cF = I.hClose
    fp = "data/nodes/"
         <> mac2Path n
         <> ".data"

mac2Path :: NodeMAC -> String
mac2Path (NodeId n) = T.unpack .  (T.replace ":" "_") . (T.replace "\"" "") $ n

nodePrefix :: (Address a) => a -> Maybe T.Text
nodePrefix n = Just $ stateTopic n

--writeDataFold :: (Monad m) =>T.Text -> FL.Fold m
writeDataFold n = FL.Fold step start en
  where
    start :: IO (I.Handle, S3.ObjectKey)
    start = do
      f <- I.openFile fp I.WriteMode
      return (f, S3.ObjectKey "")
    step :: (I.Handle, S3.ObjectKey)
         -> S3.ObjectKey
         -> IO ((I.Handle, S3.ObjectKey))
    step (h, _) (S3.ObjectKey n) =
      ((I.hPutStrLn h) . T.unpack $ n) >> (return $ (h, S3.ObjectKey $ n))
    en (h, s) = (I.hClose h) >> (print "Paths Written!") >> pure s
    fp = "data/" <> (mac2Path n) <> ".s3path"

writePathsFold n = FL.Fold step start en
  where
    start :: IO (I.Handle, S3.ObjectKey)
    start = do
      f <- I.openFile fp I.WriteMode
      return (f, S3.ObjectKey "")
    step :: (I.Handle, S3.ObjectKey)
         -> S3.ObjectKey
         -> IO ((I.Handle, S3.ObjectKey))
    step (h, _) (S3.ObjectKey n) =
      ((I.hPutStrLn h) . T.unpack $ n) >> (return $ (h, S3.ObjectKey $ n))
    en (h, s) = (I.hClose h) >> (print "Paths Written!") >> pure s
    fp = "data/" <> (show n) <> ".s3path"
