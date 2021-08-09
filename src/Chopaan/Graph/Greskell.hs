{-# LANGUAGE ConstraintKinds, PackageImports, ExplicitForAll, StandaloneDeriving, DeriveAnyClass, DeriveGeneric #-}
module Chopaan.Graph.Greskell where

import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B
import qualified "base64" Data.ByteString.Base64 as B
import Data.Time (UTCTime(..), DiffTime(..), Day(..))
--import qualified "base64" Data.Text.Encoding.Base64 as BT
import Data.Aeson (ToJSON(..), FromJSON(..), parseJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Vector as V

import Data.Char (toLower)
import Foreign.Storable.Generic
import Data.Maybe
import Data.Either
import Data.Greskell
import Data.Greskell.GraphSON.GValue (unwrapOne, unwrapAll)
import Data.Binary

type GreskellC a = (ToJSON a, FromJSON a, FromGraphSON a, Binary a, Show a)


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
encB = B.encodeBase64 . B.toStrict . encode

decB :: T.Text -> Parser (B.ByteString)
decB = (either
         (\x -> fail $ "Could not decode Base64" <> (T.unpack x))
         (pure . B.fromStrict))
       . B.decodeBase64 . T.encodeUtf8
--toBinaryTextHW = binaryJSON

instance FromGraphSON UTCTime where
  parseGraphSON = parseJSON . unwrapOne

instance FromGraphSON DiffTime where
  parseGraphSON = parseJSON . unwrapOne


deriving instance Generic UTCTime
deriving instance Generic Day
deriving instance Binary Day

-- deriving instance GStorable T.Text

-- deriving instance GStorable Bool

-- deriving instance (GStorable a) => (GStorable (Maybe a))

-- deriving instance GStorable UTCTime


