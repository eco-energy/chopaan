{-# LANGUAGE ConstraintKinds, PackageImports, ExplicitForAll #-}
module Chopaan.Graph.Greskell where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B
import qualified "base64" Data.ByteString.Base64 as B
--import qualified "base64" Data.Text.Encoding.Base64 as BT
import Data.Aeson (ToJSON(..), FromJSON(..), parseJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Vector as V

import Data.Char (toLower)


import Data.Greskell
import Data.Greskell.GraphSON.GValue (unwrapOne, unwrapAll)
import Data.Binary

type GreskellC a = (ToJSON a, FromJSON a, FromGraphSON a, Binary a)


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

parseJSONHack :: (Read a) => String -> Aeson.Value -> Parser a 
parseJSONHack x (Aeson.Array a) = do
  case (V.length a > 1) of
    True -> readValue . V.head $ a
    False -> fail $ "Empty char array"
    where
      readValue (Aeson.String s) = pure . read . T.unpack $ s
      readValue _ = fail $ x <> " is not encoded as a string"


-- $ Encode a haskell value as a base64 encoding of its

data MsgT = HW | NS deriving (Eq, Ord, Show, Enum)

binaryJSONRead :: (Binary a, Show x, ToJSON a) => x -> Aeson.Value -> Parser a
binaryJSONRead x v = case v of
  (Aeson.Array a) ->  do
    case (V.length a > 1) of
      True -> case (readValue . V.head $ a) of
        Nothing -> fail $ "failed read"
        Just a -> pure a
      False -> fail $ "Empty char array"
  _ -> fail $ " not encoded as an Array!"
  where
    readValue (Aeson.String s) = (decB s)
    readValue _ = fail $ (show x) <> " cannot decoded to desired type"
    dec (Right a) = decB a
    dec (Left a) = fail $ T.unpack a

binaryJSONWrite :: (Binary a) => a -> Aeson.Value
binaryJSONWrite a = Aeson.Array . pure . Aeson.String . encB $ a

binaryJSONEncode :: (Binary a) => a -> Aeson.Encoding
binaryJSONEncode a = toEncoding . Aeson.Array . pure . Aeson.String . encB $ a

encB :: forall a. (Binary a) => a -> T.Text
encB = B.encodeBase64 . B.toStrict . encode

decB :: forall a. (Binary a, ToJSON a) => T.Text -> Maybe a
decB = (either (const Nothing) (decode . B.fromStrict)) . B.decodeBase64 . T.encodeUtf16BE
--toBinaryTextHW = binaryJSON
