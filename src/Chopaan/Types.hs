{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings, CPP #-}
module Chopaan.Types where


import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Servant.API (FromHttpApiData(..), ToHttpApiData(..))
import qualified Data.Text as Text

import Chopaan.Hydration.Prefix
#ifndef ghcjs_HOST_OS
import RIO
import Prelude (Enum(..), read)
import RIO.Process
import RIO.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Dhall
import System.Envy
import Chopaan.Node.NodeOpts
import Options.Applicative
#else
import Prelude
import Control.DeepSeq (NFData(..))
import GHC.Generics
#endif



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

instance FromEnv Date

-- $ "mm-dd-yyyy"
instance Var Date where
  fromVar = parse
    where
      parse = (\[d, m, y] -> Date <$> d <*> m <*> y)
                   . (fmap (readMaybe . T.unpack))
                   . (take 3)
                   . T.split (== '-')
                   . T.pack
  toVar (Date d m y) = (show d) <-> (show m) <-> (show y)
    where
      a <-> b = a <> "-" <> b 

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

data InfluxConn = InfluxConn { influxHost :: !T.Text, influxPort :: !Int }
  deriving (Eq, Ord, Show, Generic)

instance FromDhall InfluxConn

icParser :: Options.Applicative.Parser InfluxConn
icParser = InfluxConn
  <$> strOption (long "influxHost" <> metavar "INFLUXHOST")
  <*> option Options.Applicative.auto (long "influxPort" <> metavar "INFLUXPORT" <> showDefault <> value 8086)

icOptions :: ParserInfo InfluxConn
icOptions = info (icParser <**> helper) $
    fullDesc <> progDesc "Chopaan"
             <> header "InfluxDB options"


-- | Command line arguments
data Options = Options
  { logVerbose :: !Bool
  , mqttOpts :: !MQTTOpts
  , nodeOpts :: ![NodeConfig]
  , kibbutzOpts :: !KibbutzOpts
  , dbOpts :: !DBOpts
  , hydrationOpts :: !HydrationOpts
  , poolConf :: !PoolConf
  , influxConn :: !InfluxConn 
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
