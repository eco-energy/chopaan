{-# LANGUAGE ConstraintKinds, PackageImports, ExplicitForAll, StandaloneDeriving, DeriveAnyClass, DeriveGeneric, OverloadedStrings, TypeApplications #-}
module Chopaan.Graph.Greskell where

import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified "base64" Data.ByteString.Base64 as B
import Data.Time (UTCTime(..), DiffTime(..), Day(..), secondsToDiffTime)
--import qualified "base64" Data.Text.Encoding.Base64 as BT
import Data.Aeson (ToJSON(..), FromJSON(..), parseJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Vector as V

import Data.Char (toLower)
import Data.Maybe
import Data.Either
import Data.Greskell
import Data.Greskell.GraphSON.GValue (unwrapOne, unwrapAll)
import Data.Binary

type GreskellC a = (ToJSON a, FromJSON a, FromGraphSON a, Binary a, Show a)


decodeBin :: (FromJSON a)
          => T.Text
          -> Either PMapLookupException BL.ByteString
          -> Either PMapLookupException a 
decodeBin _ (Left a) = (Left a)
decodeBin tag (Right x) = case Aeson.eitherDecode x of
  (Left e) -> Left (PMapParseError tag e)
  (Right x') -> Right x'


parseUnwrapTraversable :: (Traversable t, FromJSON (t GValue), FromGraphSON a)
                        => GValue -> Parser (t a)
parseUnwrapTraversable gv = traverse parseGraphSON =<< (parseJSON $ unwrapOne gv)


optSumEncoding :: String -> String -> Aeson.Options
optSumEncoding tag contents  =
  Aeson.defaultOptions
  { Aeson.constructorTagModifier = tagMod,
    Aeson.sumEncoding =
      Aeson.ObjectWithSingleField
      -- { Aeson.tagFieldName = tag,
      --   Aeson.contentsFieldName = contents
      -- }
  }
  where
    tagMod = id

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


binaryJSONRead :: (Binary a, Show x) => x -> Aeson.Value -> Parser a
binaryJSONRead x v = (pure . decode) =<< (readValue v)
  where
    readValue (Aeson.String s) = (decB s)
    readValue (Aeson.Array a) = do
      case (V.length a > 0) of
        True -> readValue . V.head $ a
        False -> fail $ "Empty char array"
    readValue _ = fail $ (show x) <> " Not a string or an array"

binaryJSONWrite :: (Binary a) => a -> Aeson.Value
binaryJSONWrite a = Aeson.String . encB $ a

binaryJSONEncode :: (Binary a) => a -> Aeson.Encoding
binaryJSONEncode a = toEncoding . Aeson.String . encB $ a

encB :: forall a. (Binary a) => a -> T.Text
encB = B.encodeBase64 . BL.toStrict . encode

decB :: T.Text -> Parser (BL.ByteString)
decB = (either
         (\x -> fail $ "Could not decode Base64" <> (T.unpack x))
         (pure . BL.fromStrict))
       . B.decodeBase64 . T.encodeUtf8
--toBinaryTextHW = binaryJSON

instance FromGraphSON UTCTime where
  parseGraphSON = parseJSON . unwrapOne

instance FromGraphSON DiffTime where
  parseGraphSON = parseJSON . unwrapOne


deriving instance Generic UTCTime
deriving instance Generic Day
deriving instance Binary Day

instance Binary UTCTime
instance Binary DiffTime where
  put a = put @Integer $ round a
  get = secondsToDiffTime <$> get

instance FromJSON B.ByteString where
  parseJSON (Aeson.String t) = pure $ (either (fail "ByteString Parse Failed!") id . B.decodeBase64 . T.encodeUtf8) t
  parseJSON _ = fail "ByteString should always be an Aeson.String!"

instance ToJSON B.ByteString where
  toJSON = Aeson.String . T.decodeUtf8 . B.encodeBase64'

instance FromJSON BL.ByteString where
  parseJSON a = (pure . BL.fromStrict) =<< Aeson.parseJSON a

instance ToJSON BL.ByteString where
  toJSON = Aeson.String . T.decodeUtf8 . B.encodeBase64' . BL.toStrict

instance FromGraphSON BL.ByteString where
  parseGraphSON = parseJSON . unwrapOne
 



-- deriving instance GStorable T.Text

-- deriving instance GStorable Bool

-- deriving instance (GStorable a) => (GStorable (Maybe a))

-- deriving instance GStorable UTCTime


