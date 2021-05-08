{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow (first, second)
import Control.Monad
import Chopaan.Comm.S3
import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz
import Chopaan.Kibbutz.Kibbutz (getNodes)
import Chopaan.Kibbutz.Mesh
import Chopaan.Node.NodeSensors
import Chopaan.Utils.Time

import Data.Greskell
import Data.Either
import Data.Bifunctor hiding (second)
import Data.ProtoLens.Encoding
import qualified Data.Text as T

import qualified Network.AWS.S3 as S3

import qualified Streamly.Prelude as S
import Streamly.Prelude (IsStream, MonadAsync, ParallelT, adapt)
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
main = S.drain $ S.trace (print) $ s3Paths bucketN (Just "/kibbutz/node/246f28a83588/state")
  where{--
    (FH.write f)
      $.|$ S.concatMap (S.unfoldr (BS.uncons))
      S.|$ S.map (BSL.toStrict . B.encode . bimap (fst) (either encodeMessage encodeMessage))
      S.|$ process
      S.|$ S.asyncly $ s3frames bucketN
      S.|$--} 
    thingType = KbtzId "kibbutz-pilot-node"
    janusHost = "localhost"
    janusPort = 8182
    writeFold = FL.Fold step start en
      where
        start :: IO (I.Handle, S3.ObjectKey)
        start = do
          f <- I.openFile "data/s3Paths.chopaan" I.WriteMode
          return (f, S3.ObjectKey "")
        step :: (I.Handle, S3.ObjectKey)
          -> S3.ObjectKey
          -> IO (FL.Step (I.Handle, S3.ObjectKey) S3.ObjectKey)
        step (h, _) (S3.ObjectKey n) =
          ((I.hPutStrLn h) . T.unpack $ n) >> (return . FL.Partial $ (h, S3.ObjectKey $ n))
        en (h, s) = (I.hClose h) >> (print "Paths Written!") >> pure s
    {--mege
    writeFileFold :: Handle -> _
    writeFileFold p = unArr . arr $ wc
      where
        unArr = FL.lmap (Ar.toStream)
        wc = FH.writeChunks p
        arr :: Fold IO Text ()  
        arr = fmap (Ar.write . (fmap (\(S3.ObjectKey n) -> n)))
    --nota = bimap nodeLinkPair id
--}

{-
addMeshFrame :: Spider n m EnergyState -> Spider NodeMAC MeshFrame -> 
addMeshFrame = undefined
o-}
