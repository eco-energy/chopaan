module Chopaan.Utils.API where
import Data.Text

toUrlPieceViaEnum :: Enum a => a -> Text
toUrlPieceViaEnum = pack . show . fromEnum

parseUrlPieceViaEnum :: Enum a => Text -> Either Text a
parseUrlPieceViaEnum = Right . toEnum . read . unpack
