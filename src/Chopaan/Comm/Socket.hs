{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, FlexibleContexts, ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE ConstraintKinds #-}

module Chopaan.Comm.Socket (subN, pubN) where

import Data.Function ((&))

import Control.Monad.Catch

import Streamly
import Streamly.Internal.Memory.Array.Types (Array)
import qualified Streamly.Prelude as S
import qualified Streamly.Network.Socket as SK
import Streamly.Network.Socket (SockSpec(..))
import Streamly.Internal.Network.Socket (handleWithM, handleWith)


import Network.Socket
import Unsafe.Coerce
import Data.Word

import Chopaan.Comm.Comm (Dispatch(..), Address(..))
import Chopaan.Node.NodeId (NodeMAC)

eth_p_all :: ProtocolNumber
eth_p_all = 0x0003


defSpec :: SockSpec
defSpec = SockSpec
       { sockFamily = AF_PACKET
       , sockType = Raw
       , sockProto = unsafeCoerce . getAddrInfo . unsafeCoerce $ eth_p_all 
       , sockOpts = []
       }

defAddr = SockAddrUnix "what" --8090 (tupleToHostAddress (0,0,0,0))

type ChannelConf = (SockSpec, SockAddr)

type NodeConf = (NodeMAC, ChannelConf)

type SocketTM t m = (IsStream t, MonadAsync m, MonadMask m)

sub' :: forall t m. SocketTM t m
  => ChannelConf -> t m (Array Word8)
sub' (spec, addr) =
  S.concatMap handleSocketRead (S.unfold SK.accept (maxListenQ, spec, addr))
  where
    handleSocketRead :: Socket -> t m (Array Word8)
    handleSocketRead = flip handleWith (S.unfold SK.readChunks)
    maxListenQ = 10


decode :: (Address n, Dispatch a) => Array Word8 -> (n, a) 
decode = undefined

sub1 :: forall t m a. (SocketTM t m, Dispatch a) => NodeConf -> t m a
sub1 (n, c) = fmap snd $ S.filter (\(n', _) -> n' == n) $ decode <$> (sub' c)

pub1 :: forall t m n a. (SocketTM t m, Address n, Dispatch a) => n -> t m a -> t m (Maybe Bool)
pub1 = undefined

subN :: (SocketTM t m, Dispatch a) => [NodeConf] -> t m (t m a)
subN = (fmap sub1) . S.fromList

pubN :: forall t m n a. (SocketTM t m, Address n, Dispatch a) => [n] -> t m (t m a) -> t m (t m (Maybe Bool))
pubN n xs = pub1 <$> S.fromList n <*> xs


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
