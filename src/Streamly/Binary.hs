{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeApplications, UndecidableInstances, QuantifiedConstraints, DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, AllowAmbiguousTypes, DefaultSignatures, ScopedTypeVariables, RankNTypes #-}
module Streamly.Binary
  ( HasEncoding(..),
    DecodeException(..),
    Bin,
    PB,
    Txt,
    Wino,
    EncT(..),
    parseMsgS,
    parseBinS,
    toPB,
    fromPB,
    toBin,
    fromBin,
    toWino,
    fromWino,
    decodeFile,
    encodeFold,
    encodeArray,
    decodeS,
    parseTextLines,
    prefixWithLength,
    toTxt,
    fromTxt,
    parseLPArray
  )
where

import Unsafe.Coerce
import GHC.Generics
import Control.Monad ((<=<))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Newtype.Generics
import Data.Bifunctor
import Data.Binary (Binary)
import Data.Bits ((.|.), unsafeShiftL)
import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Streamly.Prelude (IsStream, MonadAsync)

import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Array.Foreign.Type as A
import qualified Streamly.Internal.Data.Array.Stream.Foreign as A
import qualified Streamly.Internal.Data.Array.Stream.Fold.Foreign as AF
import qualified Streamly.Internal.Data.Parser.ParserD as P
import qualified Streamly.Internal.Data.Producer as P
import qualified Streamly.Internal.Data.Producer.Source as P
import qualified Streamly.Internal.Data.Binary.Decode as P
import qualified Streamly.External.ByteString.Lazy as SBL
import qualified Streamly.External.ByteString as SBS
import qualified Streamly.Internal.Data.Stream.IsStream as S
import Data.ProtoLens.Encoding (decodeMessage, encodeMessage)
import Data.ProtoLens.Message (Message)

import qualified Codec.Winery as W

import System.Directory (doesFileExist)
import System.Posix.Files (getFileStatus, fileSize)

data DecodeException = WinoExp W.WineryException
                     | BinExp
                     | PBExp String
                     | TxtExp
                     | NoFile
                     deriving (Generic)
                     deriving (Show)

class HasEncoding a where
  encodeA :: forall m. MonadIO m => a -> m (A.Array Word8)
  decodeA :: A.Array Word8 -> Either DecodeException a 
  chunkBytes ::  (MonadAsync m, MonadCatch m) => P.Parser m Word8 (A.Array Word8)

    
newtype Wino w = Wino { unWino :: w }
  deriving (Generic)
  deriving newtype (W.Serialise, Show)
  deriving anyclass (Newtype)

toWino :: (W.Serialise a) => a -> Wino a
toWino = Wino

fromWino :: (W.Serialise a) => Wino a -> a
fromWino = unWino

instance (W.Serialise a) => HasEncoding (Wino a) where
  encodeA = encodeLengthPrefixedBS W.serialise
  {-# INLINE encodeA #-}
  decodeA = (bimap WinoExp id) . W.deserialise . SBS.fromArray
  {-# INLINE decodeA #-}
  chunkBytes = parseLengthPrefixed

winoArray :: W.Serialise a => a -> (A.Array Word8)
winoArray = SBS.toArray . W.serialise




instance HasEncoding (A.Array Word8) where
  encodeA = pure
  {-# INLINE encodeA #-}
  decodeA = Right
  {-# INLINE decodeA #-}
  chunkBytes = parseLengthPrefixed

data EncT = BinFmt | PBFmt | TxtFmt

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
  decodeA = Right . B.decode . BL.fromStrict . SBS.fromArray
  {-# INLINE decodeA #-}
  chunkBytes = parseLengthPrefixed
  
instance Message a => HasEncoding (PB a) where
  encodeA = encodeLengthPrefixedBS (encodeMessage . unPB)
  {-# INLINE encodeA #-}
  decodeA = (bimap (PBExp) (PB)) . decodeMessage . SBS.fromArray
  {-# INLINE decodeA #-}
  chunkBytes = parseLengthPrefixed
  
instance HasEncoding (Txt a) where
  encodeA = pure . SBS.toArray . T.encodeUtf8 . (T.unlines . pure) . unTxt
  {-# INLINE encodeA #-}
  decodeA = Right . Txt . T.decodeUtf8 . SBS.fromArray
  {-# INLINE decodeA #-}
  chunkBytes = parseNewline
  {-# INLINE chunkBytes#-}

prefixWithLength :: (MonadIO m) => Int -> A.Array Word8 -> m (A.Array Word8)
prefixWithLength l y = do
  prefix <- A.toArray (SBL.toChunks (B.runPut . B.put $ l))
  A.toArray $ S.fromList [prefix, y]
{-# INLINE prefixWithLength #-}


prefixLengthArray :: (MonadIO m) => A.Array Word8 -> m (A.Array Word8)
prefixLengthArray y = do
  let l = A.byteLength y
  prefix <- A.toArray (SBL.toChunks (B.runPut . B.put $ l))
  A.toArray $ S.fromList [prefix, y]
{-# INLINE prefixLengthArray #-}

-- | Encode stream of elements using 'Put' from 'Binary'.
-- Resulting bytestrings are not guaranteed to be aligned in any way.
encodeLengthPrefixedBS :: (MonadIO m) => (a -> BS.ByteString) -> a -> m (A.Array Word8)
encodeLengthPrefixedBS toBS = prefixLengthArray . SBS.toArray . toBS
{-# INLINE encodeLengthPrefixedBS #-}

encodeLengthPrefixedBL :: (MonadIO m) => (a -> BL.ByteString) -> a -> m (A.Array Word8)
encodeLengthPrefixedBL toBL a = prefixLengthArray =<< (A.toArray $ SBL.toChunks (toBL a))
{-# INLINE encodeLengthPrefixedBL #-}


{-# INLINE word64beD #-}
word64beD :: MonadCatch m => P.Parser m Word8 Word64
word64beD = P.Parser step initial extract
    where
    initial = return $ P.IPartial $ Tuple' 0 56
    step (Tuple' w sh) a = return $
        if sh /= 0
        then
            let w1 = w .|. (fromIntegral a `unsafeShiftL` sh)
             in P.Continue 0 (Tuple' w1 (sh - 8))
        else P.Done 0 (w .|. fromIntegral a)
    extract _ = throwM $ P.ParseError "word64beD: end of input"

parseLengthPrefixed :: (MonadIO m, MonadCatch m) => P.Parser m Word8 (A.Array Word8)  
parseLengthPrefixed = (\l -> P.takeEQ l (A.writeN l)) =<< (fromIntegral <$> word64beD)
{-# INLINE parseLengthPrefixed #-}

parseLPArray :: (MonadIO m, MonadCatch m) => AF.Fold m Word8 (A.Array Word8)
parseLPArray = AF.fromParser parseLengthPrefixed
{-# INLINE parseLPArray #-}

parseNewline :: (MonadIO m, MonadCatch m) => P.Parser m Word8 (A.Array Word8)
parseNewline = P.wordBy nl (A.write)
  where
    nl :: Word8 -> Bool
    nl = (== '\n') . unsafeCoerce
{-# INLINE parseNewline #-}

decodeS :: forall t m a. (HasEncoding a, IsStream t, MonadAsync m, MonadCatch m)
  => t m Word8 -> t m (Either DecodeException a)
decodeS = (fmap decodeA) . (S.parseManyD (chunkBytes @a))
{-# INLINE decodeS #-}

decodeFile :: forall t m a. (HasEncoding a, IsStream t, MonadAsync m, MonadCatch m)
  => FilePath -> t m (Either DecodeException a)
decodeFile f = S.concatM $ do
  exists <- liftIO $ doesFileExist f
  case (exists) of
    True -> return $ S.mapM (pure . decodeA) . (S.parseManyD (chunkBytes @a)) . File.toBytes $ f
    False -> return $ S.fromPure (Left NoFile)
{-# INLINE decodeFile #-}


type FileSource a = (P.Source FilePath a)

decodeUnfold :: forall m a. (HasEncoding a, MonadAsync m, MonadCatch m)
  => P.Producer m (FileSource Word8) Word8
  -> UF.Unfold m (FileSource Word8) (Either DecodeException a)
decodeUnfold = P.simplify . decodeProducer

decodeProducer :: forall m a. (HasEncoding a, MonadAsync m, MonadCatch m)
  => P.Producer m (FileSource Word8) Word8
  -> P.Producer m (FileSource Word8) (Either DecodeException a)
decodeProducer = (fmap decodeA) . P.parseManyD (chunkBytes @a)

encodeArray :: (HasEncoding a, MonadAsync m, MonadCatch m)
  => FilePath -> a -> m ()
encodeArray fp = liftIO . (File.putChunk fp <=< encodeA)
{-# INLINE encodeArray #-}

encodeFold :: (HasEncoding a, MonadAsync m, MonadCatch m)
  => FilePath -> FL.Fold m a ()
encodeFold fp = FL.lmapM encodeA (File.writeChunks fp)
{-# INLINE encodeFold #-}

encodeFile :: (HasEncoding a, MonadAsync m, MonadCatch m)
  => FilePath -> FL.Fold m a ()
encodeFile fp = FL.lmapM encodeA (File.writeChunks fp)
{-# INLINE encodeFile #-}

-- encodeFold2 :: (HasEncoding a, MonadAsync m, MonadCatch m)
--   => FilePath -> FL.Fold m a ()
-- encodeFold2 fp = FL.lmapM encodeA (File.writeChunks2 fp)
-- {-# INLINE encodeFold2 #-}


parseMsgS :: (IsStream t, MonadAsync m, Message a, MonadCatch m)
  => FilePath -> t m (Either DecodeException (PB a))
parseMsgS = decodeFile
{-# INLINE parseMsgS #-}

parseBinS :: (IsStream t, MonadAsync m, Binary a, MonadCatch m)
  => FilePath -> t m (Either DecodeException (Bin a))
parseBinS = decodeFile
{-# INLINE parseBinS #-}

parseTextLines :: (IsStream t, MonadAsync m, MonadCatch m)
  => FilePath -> t m (Either DecodeException T.Text)
parseTextLines = (fmap (second fromTxt)) . decodeFile
{-# INLINE parseTextLines #-}
