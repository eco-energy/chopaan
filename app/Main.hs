import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_chopaan

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_chopaan.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run

{--

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

-- Protobuf Imports
import Proto.NodeMessages as NM
import Proto.NodeMessages_Fields as NM
import Data.ProtoLens (defMessage, showMessage, encodeMessage, decodeMessage)
import Lens.Micro
import qualified Data.Text as Text
import qualified Data.ByteString as BS



-- MQTT Imports
import qualified Network.MQTT.Client as MQ
import qualified Network.MQTT.Topic as MQ
import Network.MQTT.Types (ConnACKFlags (..))
import Network.Connection
import Network.TLS
import Data.X509.CertificateStore
import Data.Default.Class
import Network.TLS.Extra.Cipher
import Network.URI
import Control.Exception (Handler (..), IOException, catches)
import Control.Monad (forever, when, liftM)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy as BL

import Control.Concurrent.STM
import Control.Concurrent

-- Vis and CLI
import qualified Text.PrettyPrint.Tabulate as PPT
import GHC.Generics (Generic)
import Data.Data

-- Energy Transaction Stuff
import qualified Data.Time as Time
import Data.Time.Lens
import Data.ULID (getULID)
import Data.Convertible
import Control.Concurrent.STM.TQueue





class Frameable a where
  toMeshFrame :: a -> NM.MeshFrame
  fromMeshFrame :: NM.MeshFrame -> Maybe a


instance Frameable NM.EnergyTransactionRequest where
  toMeshFrame etr = undefined
  fromMeshFrame m = undefined
--}

{--
data Transaction = Transaction
  { power :: Watts
  , edge :: (Int, Int)
  , time :: Integer
  , edgeCost :: Watts
  } deriving (Eq, Ord, Show, Generic)
--}




----------------------------------------------------------------------------------





{--initTransaction 

runEnergyTransactor ts = undefined






configureNode :: NodeId -> (NM.BatteryParameters, NM.PVParameters) -> NM.MeshFrame
configureNode = undefined



data ChopaanOpts = ChopaanOpts
  { mqttURI :: Text.Text
  , connId :: Text.Text
  , thingTypeName :: Text.Text
  }


data TransactionOpts = TransactionOpts
  { sources :: [Text.Text],
    sinks :: [Text.Text]
  }


--}
