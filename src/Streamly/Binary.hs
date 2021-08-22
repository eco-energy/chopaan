{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Streamly.Binary
  ( decode',
    decodeFileS,
    encode',
    encodeFold,
  )
where

import System.IO.Unsafe
import Control.Monad.Catch
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)

import Streamly.Prelude (SerialT, IsStream, MonadAsync)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.FileSystem.File as FL
import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Array.Stream.Foreign as A
import qualified Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Data.Binary.Decode as P
import qualified Streamly.External.ByteString.Lazy as SBL
import qualified Streamly.External.ByteString as SBS
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Prelude as S

-- | Encode stream of elements using 'Put' from 'Binary'.
-- Resulting bytestrings are not guaranteed to be aligned in any way.
encodeLengthPrefixedBinary :: (MonadAsync m, MonadFail m) => (a -> Put) -> a -> m (A.Array Word8)
encodeLengthPrefixedBinary p a = do
  let x = runPut . p $ a
  y <- A.toArray $ SBL.toChunks x
  let l = A.length y
  prefix <- A.toArray (SBL.toChunks (runPut . put $ l))
  --liftIO $ print prefix
  --liftIO $ print l
  z <- A.toArray $ S.fromList [prefix, y]
  liftIO $ print z
  return z

parseLengthPrefixedBinary :: (MonadAsync m, MonadCatch m) => P.Parser m Word8 (A.Array Word8)  
parseLengthPrefixedBinary = do
  len <- P.word64be
  let len' = fromIntegral len
  z <- P.takeEQ len' (A.writeN (len'))
  let deb = unsafePerformIO $ do
        print $ "decoded length: " <> (show len)
        print $ "read into: " <> (show z)
  deb `seq` (return z)
  
decodeArray :: (IsStream t, MonadAsync m, MonadCatch m, MonadFail m)
  => t m (Word8) -> t m (A.Array Word8)
decodeArray = S.parseMany parseLengthPrefixedBinary

encode' :: (IsStream t, MonadAsync m, MonadCatch m, MonadFail m, Binary a)
  => t m a -> t m (A.Array Word8)
encode' = S.mapM (encodeLengthPrefixedBinary put)

decode' :: (IsStream t, MonadAsync m, MonadCatch m, MonadFail m, Binary a)
  => t m Word8 -> t m a
decode' = S.map (decode . BL.fromStrict . SBS.fromArray) . decodeArray

decodeFileS :: (Binary a, IsStream t, MonadAsync m, MonadFail m, MonadCatch m) => FilePath -> t m a
decodeFileS fp = decode' (FL.toBytes fp)

encodeFold :: (Binary a, MonadAsync m, MonadFail m, MonadCatch m)
  => FilePath -> FL.Fold m a ()
encodeFold fp = FL.lmapM (encodeLengthPrefixedBinary put) (FL.writeChunks fp)
