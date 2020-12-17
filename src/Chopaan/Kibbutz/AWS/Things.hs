{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Chopaan.Kibbutz.AWS.Things where


import Chopaan.Node.NodeId
import qualified Data.Text as Text
import qualified Data.ByteString as BS

import qualified Network.MQTT.Topic as MQ

import Lens.Micro

-- AWS Imports
import qualified Network.AWS.IoT.ListThings as Iot
import qualified Network.AWS.IoT.Types as Iot
import Control.Monad.Trans.AWS
import Data.Maybe
import System.IO

-- Streamly
import Streamly ()
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold.Types as UF
import qualified Streamly.Internal.Data.Stream.StreamD.Type as STy

nameToTopic :: Text.Text -> ThingName -> MQ.Topic
nameToTopic suffix name = prefix <> n <> suffix
  where
    prefix = "/kibbutz/node/"
    n = Text.replace ":" "" name

topicToNodeId :: Text.Text -> Text.Text -> Maybe NodeMAC
topicToNodeId suffix t =
  case (isValidTopic t) of
       False  -> Nothing
       _ -> Just (NodeId n)
  where
    n = colonize $ Text.replace suffix "" $ Text.replace prefix "" t
    isValidTopic t' = prefix `Text.isPrefixOf` t' && suffix `Text.isSuffixOf` t'
    prefix = "/kibbutz/node/"
    colonize :: Text.Text -> Text.Text
    colonize cs = Text.intercalate i $ Text.chunksOf 2 cs
      where
        i = ":"


{--------------------------------------------------------------------------------------------------------

                   Thing Tings and Rules for Topics
---------------------------------------------------------------------------------------------------------}


thingName :: Iot.ThingAttribute -> Maybe ThingName
thingName t = t ^. Iot.taThingName

iot :: BS.ByteString -> Service
iot svc = Iot.ioT{_svcPrefix=svc} :: Service

getThings :: Text.Text -> IO [Iot.ThingAttribute]
getThings thingTypeName = do
  let
    iiot = iot "execute-api"
    req = (Iot.listThings & Iot.ltThingTypeName .~ (Just thingTypeName))
  lgr <- newLogger Debug stdout
  env <- newEnv Discover <&> set envLogger lgr . set envRegion Singapore <&> configure iiot  
  runResourceT . runAWST env $ do
    things <- S.toList
      $ S.map (\x -> x ^. Iot.ltrsThings)
      $ S.unfold pageUF req
    return $ concat things

pageUF :: forall m a r. (AWSPager a, AWSConstraint r m) => UF.Unfold m a (Rs a)
pageUF = UF.Unfold step inject
  where
    step :: Maybe a -> m (STy.Step (Maybe a) (Rs a)) 
    step (Just req) = do
      y <- send req
      return $ STy.Yield y (page req y)
    step Nothing = do
      return $ STy.Stop
    inject :: a -> m (Maybe a)
    inject = pure . Just
