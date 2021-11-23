{-# LANGUAGE ConstraintKinds, PackageImports, ExplicitForAll, StandaloneDeriving, DeriveAnyClass, DeriveGeneric, OverloadedStrings, TypeApplications, ScopedTypeVariables, DerivingVia #-}
module Chopaan.Graph.Greskell where

import GHC.Generics
import Control.Monad
import Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified "base64" Data.ByteString.Base64 as B
import Data.Time (UTCTime(..), NominalDiffTime(..), Day(..), secondsToDiffTime)
--import qualified "base64" Data.Text.Encoding.Base64 as BT
import Data.Aeson (ToJSON(..), FromJSON(..), parseJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Vector as V

import qualified Codec.Winery as W
import Data.Char (toLower)
import Data.Maybe
import Data.Either
import Data.Greskell
import Data.Greskell.GraphSON.GValue (unwrapOne, unwrapAll)
import Data.Binary

type GreskellC a = (ToJSON a, FromJSON a, FromGraphSON a, W.Serialise a, Show a)


decodeBin :: (W.Serialise a)
          => T.Text
          -> Either PMapLookupException Aeson.Value
          -> Parser a 
decodeBin tag (Left a) = fail $ (show tag) <> ":- " <> (show a)
decodeBin tag (Right x) = wineryJSONRead tag x


parseUnwrapTraversable :: (Traversable t, FromJSON (t GValue), FromGraphSON a)
                        => GValue -> Parser (t a)
parseUnwrapTraversable gv = traverse parseGraphSON =<< (parseJSON $ unwrapOne gv)


optSumEncoding :: Aeson.Options
optSumEncoding = Aeson.defaultOptions { Aeson.sumEncoding = Aeson.ObjectWithSingleField }

-- $ JANUSGRAPH DOES NOT SUPPORT NESTED PROPERTY TYPES. THIS IS AN UGLY HACK TO
-- $ SERIALIZE NESTED THINGS HORRIBLY. IT SHOULD BE MOVED OUT OF THE FromJSON, ToJSON INSTANCES!

toJSONHack :: (Show a) => a -> Aeson.Value
toJSONHack = toJSON . T.pack . show

toEncodingHack :: (Show a) => a -> Aeson.Encoding
toEncodingHack = toEncoding . T.pack . show

-- parseJSONHack :: (Read a) => String -> Aeson.Value -> Parser a 
-- parseJSONHack x (Aeson.Array a) = do
--   case (V.length a => 1) of
--     True -> readValue . V.head $ a
--     False -> fail $ "Empty char array"
--     where
--       readValue (Aeson.String s) = pure . read . T.unpack $ s
--       readValue _ = fail $ x <> " is not encoded as a string"


-- $ Convert a haskell value to a base64 encoded string inside Aeson.


wineryJSONRead :: (W.Serialise a, Show x) => x -> Aeson.Value -> Parser a
wineryJSONRead x v = readValue v
  where
    readValue (Aeson.String s) = (decB s)
    readValue (Aeson.Array a) = do
      case (V.length a > 0) of
        True -> readValue . V.head $ a
        False -> fail $ "Empty char array"
    readValue _ = fail $ (show x) <> " Not a string or an array"

wineryJSONWrite :: (W.Serialise a) => a -> Aeson.Value
wineryJSONWrite a = Aeson.String . encB $ a

wineryJSONEncode :: (W.Serialise a) => a -> Aeson.Encoding
wineryJSONEncode a = toEncoding . Aeson.String . encB $ a


encB :: forall a. (W.Serialise a) => a -> T.Text
encB = B.encodeBase64 . W.serialise

data DecodeError = B64E T.Text | WE W.WineryException
  deriving (Show)

decB :: forall a. (W.Serialise a) => T.Text -> Parser a
decB =  either (fail . show) (pure) . join . dec' . (B.decodeBase64 . T.encodeUtf8)
  where
    dec' :: Either T.Text B.ByteString -> Either DecodeError (Either DecodeError a) 
    dec' = bimap (B64E) dec
    dec :: B.ByteString -> Either DecodeError a
    dec = (first WE . W.deserialise)
--toW.SerialiseTextHW = wineryJSON

instance FromGraphSON UTCTime where
  parseGraphSON = parseJSON . unwrapOne

instance FromGraphSON NominalDiffTime where
  parseGraphSON = parseJSON . unwrapOne


deriving instance Generic UTCTime
deriving instance Generic Day
deriving via (W.WineryRecord Day) instance W.Serialise Day


-- instance FromJSON B.ByteString where
--   parseJSON (Aeson.String t) = pure $
--     ((either (fail "ByteString Parse Failed!") id) . B.decodeBase64 . T.encodeUtf8) t
--   parseJSON _ = fail "ByteString should always be an Aeson.String!"

-- instance ToJSON B.ByteString where
--   toJSON = Aeson.String . T.decodeUtf8 . B.encodeBase64'

-- instance FromJSON BL.ByteString where
--   parseJSON a = (pure . BL.fromStrict) =<< Aeson.parseJSON a

-- instance ToJSON BL.ByteString where
--   toJSON = Aeson.String . T.decodeUtf8 . B.encodeBase64' . BL.toStrict

-- instance FromGraphSON BL.ByteString where
--   parseGraphSON = parseJSON . unwrapOne
 



-- deriving instance GStorable T.Text

-- deriving instance GStorable Bool

-- deriving instance (GStorable a) => (GStorable (Maybe a))

-- deriving instance GStorable UTCTime


