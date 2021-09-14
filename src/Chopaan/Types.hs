{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings, CPP #-}
module Chopaan.Types where


import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Servant.API (FromHttpApiData(..), ToHttpApiData(..))
import qualified Data.Text as Text
#ifndef ghcjs_HOST_OS
import RIO
import Prelude (Enum(..), read)
import RIO.Process
import RIO.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Dhall
import Chopaan.Node.NodeOpts
#else
import Prelude
import Control.DeepSeq (NFData(..))
import GHC.Generics
#endif

data Resolution = Year | Month | Week | Day | Hour | Minute | Second
  deriving (Eq, Ord, Show, Generic, Bounded, Enum, NFData, ToJSON, FromJSON)

toUrlPieceViaEnum :: Enum a => a -> Text
toUrlPieceViaEnum = Text.pack . show . fromEnum

parseUrlPieceViaEnum :: Enum a => Text -> Either Text a
parseUrlPieceViaEnum = Right . toEnum . read . Text.unpack

instance ToHttpApiData Resolution where
  toUrlPiece = toUrlPieceViaEnum

instance FromHttpApiData Resolution where
  parseUrlPiece = parseUrlPieceViaEnum

#ifndef ghcjs_HOST_OS
data DBOpts = DBOpts
  { host :: !Text
  , port :: !Integer
  , database :: !Text
  , user :: !Text
  , password :: !Text
  } deriving (Eq, Ord, Show, Generic)

instance FromDhall DBOpts

data MQTTOpts = MQTTOpts
  { mqttURI :: !Text
  , certPath :: !FilePath
  , keyPath :: !FilePath
  , caPath :: !FilePath
  } deriving (Eq, Ord, Show, Generic)

data KibbutzOpts = KibbutzOpts
  { name :: !Text
  } deriving (Generic, Show)


instance FromDhall MQTTOpts
instance FromDhall KibbutzOpts

data Date = Date
  { day :: !Integer
  , month :: !Integer
  , year :: !Integer
  } deriving (Generic, Show)

toUTC :: Date -> UTCTime
toUTC d = UTCTime (fromGregorian
                   (fromIntegral . year $ d)
                   (fromIntegral . month $ d)
                   (fromIntegral . day $ d))
          (secondsToDiffTime 0)

instance FromDhall Date


instance FromDhall Resolution

data BufferingOpts = BufferingOpts
  { prefixBuffer :: !Int
  , pathBuffer :: !Int
  , frameBuffer :: !Int
  , nodeBuffer :: !Int
  } deriving (Generic, Show)

instance FromDhall BufferingOpts

data HydrationOpts = HydrationOpts
  { start :: !Date
  , end :: !Date
  , s3BucketName :: !Text
  , dbSave :: !Bool
  , resolution :: !Resolution
  , bufOpts :: !BufferingOpts
  , hPrefix :: !Text
  } deriving (Generic, Show)

instance FromDhall HydrationOpts

data PoolConf = PoolConf
  { pNumStripes :: !Int
  , reaperWait :: !Double
  , maxConnsPerStripe :: !Int
  } deriving (Generic, Show)

instance FromDhall PoolConf

-- | Command line arguments
data Options = Options
  { logVerbose :: !Bool
  , mqttOpts :: !MQTTOpts
  , nodeOpts :: ![NodeConfig]
  , kibbutzOpts :: !KibbutzOpts
  , dbOpts :: !DBOpts
  , hydrationOpts :: !HydrationOpts
  , poolConf :: !PoolConf
  } deriving (Generic, Show)

instance FromDhall Options

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
#endif
