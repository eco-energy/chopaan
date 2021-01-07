{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, FlexibleContexts, ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings#-}

module Chopaan.Comm.Socket (sub, pub) where

import Chopaan.Comm.Comm (Dispatch(..), Address(..))
import Chopaan.Node.NodeId (NodeMAC)

import Streamly
import qualified Streamly.Prelude as P


data SocketErr

sub :: forall t m a. (IsStream t, MonadAsync m) => NodeMAC -> m (t m a)
sub = undefined

pub :: forall t m a. (IsStream t, MonadAsync m) => t m a -> t m (Maybe SocketErr)
pub = undefined


{--
-- Different string modules should be unified under one interface
import qualified Data.Text as Text 
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BSC



import qualified Network.MQTT.Client as MQ
import qualified Network.MQTT.Topic as MQ ()
import qualified Network.MQTT.Types as MQTy

import Network.Connection
import Network.TLS
import Data.X509.CertificateStore (readCertificateStore)
import Data.X509.Validation (validateDefault)

import Data.Default.Class
import Data.Maybe (fromJust)
import Network.TLS.Extra.Cipher
import Network.URI

import Control.Exception (Handler (..), catches)
import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM

import Data.ProtoLens (encodeMessage)

import Chopaan.Types (MQTTOpts(..))
import Chopaan.Comm.Comm (Address(..), Dispatch(..), PubQueue)
import Chopaan.Comm.Queues (NodeQueue(..))
import Chopaan.Utils.Retry
import Proto.NodeMessageSchema.NodeMessages (MeshFrame)

import Control.Retry

-- I want to listen on a unix socket for a macAddr and a protobuf encoded payload.



-- https://stackoverflow.com/questions/40081508/how-to-provide-a-client-certificate-to-http-client-tls
mkTLSSettings :: FilePath -> FilePath -> FilePath -> Text.Text -> Text.Text -> IO TLSSettings
mkTLSSettings cert key caPath hostName name = do
  creds <- either (error "Client Certificate Not Found") Just <$> credentialLoadX509 cert key
  --caCreds <- fromJust (error "CA Certificate Not Found") (readCertificateStore caPath)
  let
    hooks = def { onCertificateRequest = \_ -> return creds
                , onServerCertificate = \_ _ _ _ -> mempty -- validateDefault caCreds a b c
                }
    clientParams = (defaultParamsClient (Text.unpack hostName :: HostName) ((BSC.pack . Text.unpack) name))
                  { clientHooks=hooks
                  , clientSupported = def {supportedCiphers=ciphersuite_strong}
                  }
  return (TLSSettings clientParams)



--}
