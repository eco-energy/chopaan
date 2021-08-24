{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeApplications, UndecidableInstances, QuantifiedConstraints, DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, AllowAmbiguousTypes, DefaultSignatures, ScopedTypeVariables #-}
module Streamly.Binary
  ( HasEncoding(..),
    Bin,
    PB,
    Txt,
    parseMsgS,
    parseBinS,
    toPB,
    fromPB,
    toBin,
    fromBin,
    decodeFile,
    encodeFold,
    parseTextLines,
    toTxt,
    fromTxt
  )
where

import Unsafe.Coerce
import GHC.Generics
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Newtype.Generics
import Data.Binary (Binary)
import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Streamly.Prelude (IsStream, MonadAsync)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.FileSystem.File as FL
import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Array.Foreign.Type as A
import qualified Streamly.Internal.Data.Array.Stream.Foreign as A
import qualified Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Data.Binary.Decode as P
import qualified Streamly.External.ByteString.Lazy as SBL
import qualified Streamly.External.ByteString as SBS
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Prelude as S
import Data.ProtoLens.Encoding (decodeMessage, encodeMessage)
import Data.ProtoLens.Message (Message)

class HasEncoding a where
  encodeA :: forall m. MonadIO m => a -> m (A.Array Word8)
  decodeA :: A.Array Word8 -> Maybe a 
  chunkBytes ::  (MonadAsync m, MonadCatch m) => P.Parser m Word8 (A.Array Word8)
  default chunkBytes :: (MonadAsync m, MonadCatch m) => P.Parser m Word8 (A.Array Word8)
  chunkBytes = parseLengthPrefixed
  {-# INLINE chunkBytes #-}
  
newtype Bin a = Bin { unBin :: a }
  deriving (Generic)
  deriving newtype Binary
  deriving anyclass Newtype

newtype PB a = PB { unPB :: a }
  deriving Generic
  deriving anyclass Newtype

newtype Txt a = Txt { unTxt :: T.Text }
  deriving Generic
  deriving anyclass Newtype

toPB :: (Message a) => a -> PB a
toPB = PB

toBin :: (Binary a) => a -> Bin a
toBin = Bin

fromBin :: (Binary a) => Bin a -> a
fromBin = unBin

fromPB :: (Message a) => PB a -> a
fromPB = unPB

toTxt :: T.Text -> Txt a
toTxt = Txt

fromTxt :: Txt a -> T.Text
fromTxt = unTxt

toTxtViaS :: (Show a) => a -> Txt a
toTxtViaS = Txt . T.pack . show


instance (Binary a) => HasEncoding (Bin a) where
  encodeA = encodeLengthPrefixedBL (B.runPut . B.put)
  {-# INLINE encodeA #-}
  decodeA = B.decode . BL.fromStrict . SBS.fromArray
  {-# INLINE decodeA #-}
  
instance Message a => HasEncoding (PB a) where
  encodeA = encodeLengthPrefixedBS (encodeMessage . unPB)
  {-# INLINE encodeA #-}
  decodeA = (either (const Nothing) (Just .  PB)) . decodeMessage . SBS.fromArray
  {-# INLINE decodeA #-}
  
instance HasEncoding (Txt a) where
  encodeA = pure . SBS.toArray . T.encodeUtf8 . (T.unlines . pure) . unTxt
  {-# INLINE encodeA #-}
  decodeA = Just . Txt . T.decodeUtf8 . SBS.fromArray
  {-# INLINE decodeA #-}
  chunkBytes = parseNewline
  {-# INLINE chunkBytes#-}

prefixLengthArray :: (MonadIO m) => A.Array Word8 -> m (A.Array Word8)
prefixLengthArray y = do
  let l = A.byteLength y
  prefix <- A.toArray (SBL.toChunks (B.runPut . B.put $ l))
  A.toArray $ S.fromList [prefix, y]
{-# INLINE prefixLengthArray #-}

-- | Encode stream of elements using 'Put' from 'Binary'.
-- Resulting bytestrings are not guaranteed to be aligned in any way.
encodeLengthPrefixedBS :: (MonadIO m) => (a -> BS.ByteString) -> a -> m (A.Array Word8)
encodeLengthPrefixedBS toBS =
  prefixLengthArray . SBS.toArray . toBS --SBL.toChunks (runPut . p $ a))
{-# INLINE encodeLengthPrefixedBS #-}

encodeLengthPrefixedBL :: (MonadIO m) => (a -> BL.ByteString) -> a -> m (A.Array Word8)
encodeLengthPrefixedBL toBL a =
  prefixLengthArray =<< (A.toArray $ SBL.toChunks (toBL a))
{-# INLINE encodeLengthPrefixedBL #-}
  
parseLengthPrefixed :: (MonadIO m, MonadCatch m) => P.Parser m Word8 (A.Array Word8)  
parseLengthPrefixed = do
  len <- P.word64be
  let len' = fromIntegral len
  z <- P.takeEQ len' (A.writeN (len'))
  -- let deb = unsafePerformIO $ do
  --       print $ "decoded length: " <> (show len)
  --       print $ "read into: " <> (show z)
  -- deb `seq` 
  (return z)
{-# INLINE parseLengthPrefixed #-}

parseNewline :: (MonadIO m, MonadCatch m) => P.Parser m Word8 (A.Array Word8)
parseNewline = P.wordBy nl (A.write)
  where
    nl :: Word8 -> Bool
    nl = (== '\n') . unsafeCoerce
{-# INLINE parseNewline #-}

decodeFile :: forall t m a. (HasEncoding a, IsStream t, MonadAsync m, MonadCatch m) => FilePath -> t m (Maybe a)
decodeFile = (fmap decodeA) . (S.parseMany (chunkBytes @a)) . FL.toBytes
{-# INLINE decodeFile #-}

encodeFold :: (HasEncoding a, MonadAsync m, MonadCatch m)
  => FilePath -> FL.Fold m a ()
encodeFold fp = FL.lmapM (encodeA) (FL.writeChunks fp)
{-# INLINE encodeFold #-}

parseMsgS :: (IsStream t, MonadAsync m, Message a, MonadCatch m) => FilePath -> t m (Maybe (PB a))
parseMsgS = decodeFile

parseBinS :: (IsStream t, MonadAsync m, Binary a, MonadCatch m) => FilePath -> t m (Maybe (Bin a))
parseBinS = decodeFile

parseTextLines :: (IsStream t, MonadAsync m, MonadCatch m) => FilePath -> t m (Maybe (Txt a))
parseTextLines = decodeFile
