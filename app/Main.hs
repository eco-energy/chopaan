{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Proto.NodeMessages as NM
import Proto.NodeMessages_Fields as NM
import Data.ProtoLens (defMessage, showMessage, encodeMessage, decodeMessage)
import Lens.Micro
import Data.Word
import qualified Data.Text as Text
import qualified Data.ByteString as BS


-- AWS Imports
import qualified Network.AWS.IoT.ListThings as Iot
import qualified Network.AWS.IoT.Types as Iot
import Control.Monad.Trans.AWS
import Data.Maybe
import System.IO

{--
type KibbutzName = Text
type StateTopic = Text
type ControlTopic = Text

kbtz :: KibbutzName
kbtz = "PILOT"
--}
{--
getNodes :: KibbutzName -> IO [(StateTopic, ControlTopic)]
getNodes kbtz = do
  let
    query = Iot.listThings & Iot.ltAttributeValue kbtz
    thingName ta = ta ^. Iot.taThingName
    toTopics :: Text -> (StateTopic, ControlTopic)
    toTopics tn = (pfx++tn++"/state", pfx++tn++"/control")
    pfx = "kibbutz/node/"
  lgr <- newLogger Trace stdout
  env <- newEnv Discover <&> set envLogger lgr . set envRegion ("ap-southeast-1" :: Region)
  runResourceT . runAWST env $ do
    things <- send query
    

eTR :: NM.EnergyTransactionRequest
eTR =
  defMessage
      & uuid .~ ("123456" :: Text)
      & dispatchedAt .~ (223123123 :: Word64)
      & powerInWatts .~ (100 :: Double)
      & durationInSeconds .~ (60*60 :: Word64)
      & direction .~ Outgoing


meshFrame :: NM.MeshFrame
meshFrame =
  defMessage
      & time .~ (3424234453 :: Word64)
      & transaction .~ eTR
--}

-- I want to setup an MQTT client that subscribes to kibuttz/node/{mac}/state and publishes to /kibbutz/node/{mac}/control
-- We obtain the list of nodes for a particular kibbutz by using the kibbutz name and querying AWS Iot for all thing names
-- for things which belong to that kibbutz.


type StateTopic = Text.Text
type ControlTopic = Text.Text

type ThingName = Text.Text

attrName :: Maybe Text.Text
attrName = Just "kibbutz"

kbtz :: Maybe Text.Text
kbtz = Just "PILOT"

ttn :: Maybe Text.Text
ttn = Just "kibbutz-pilot-node"

iiot :: Service
iiot = Iot.ioT{_svcPrefix="execute-api"}

getThings :: IO [Iot.ThingAttribute]
getThings = do
    lgr <- newLogger Trace stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion Singapore <&> configure iiot
    runResourceT . runAWST env $ do
      things <- send (Iot.listThings & Iot.ltThingTypeName .~ ttn)
      return $ things ^. Iot.ltrsThings

thingName :: Iot.ThingAttribute -> Maybe Text.Text
thingName t = t ^. Iot.taThingName

nameToTopics :: ThingName -> (StateTopic, ControlTopic)
nameToTopics name = (st, ct)
  where
    st :: StateTopic
    st = prefix <> n <> stChannel
    ct :: ControlTopic
    ct = prefix <> n <> cChannel
    stChannel = "/state"
    cChannel = "/control"
    prefix = "kibbutz/node/"
    n = Text.replace ":" "" name



main :: IO ()
main = do
  things <- getThings
  let
    topics = (fmap nameToTopics) <$> map thingName things
  putStrLn (show (map (\t -> (fromMaybe ("NotFound", "NotFound") t)) topics))


{--
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
--}
