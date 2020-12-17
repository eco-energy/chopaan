{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
module Chopaan.Kibbutz.Serve where

import Network.WebSockets

import Chopaan.Node.NodeId
import Chopaan.Kibbutz.Kibbutz
import Chopaan.Comm.Comm (Address(..), Dispatch(..))


import qualified Data.ProtoLens as PB
import Data.Text (pack)
import qualified Data.Map.Lazy as M


import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent


import Streamly
import qualified Streamly.Prelude as S


import Servant.Server
import Servant.API
import Servant.API.WebSocket
import Servant.JS


{--
connOpts :: ConnectionOptions
connOpts = defaultConnectionOptions


instance PB.Message a => WebSocketsData a where
  fromDataMessage = undefined
  fromLazyByteString = undefined
  toLazyByteString = undefined


serveKbtz :: (IsStream t, MonadAsync m, Address n, Dispatch a) => Kbtz t m n a -> m ()
serveKbtz k = do
  S.mapM_ (liftIO . (sendBinaryData undefined)) $ adapt . runKbtz $ k
--}

type NodeAPI n a = "node" :> QueryParam "nodeId" n :> WebSocket

type KbtzAPI n a = "kbtz" :> QueryParam "kbtzId" n :> Get '[JSON] [n]

type API n a = KbtzAPI n a :<|> NodeAPI n a


server :: forall t m n a. (KbtzConn t m n, Dispatch a) => Kbtz t m n a -> ServerT (API n a) m
server kb@(Kbtz k) = kbtzData :<|> streamData
  where
    log :: MonadIO m => String -> m ()
    log = liftIO . print
    kbtzData :: MonadIO m => Maybe n -> m ([n])
    kbtzData (Just _) = return . nodes $ kb 
    kbtzData Nothing = log "No Kbtz Id" >> return mempty
    streamData :: MonadIO m => Maybe n -> Connection ->  m ()
    streamData (Just n) c = do
      let nodeS = M.lookup n k
      case nodeS of
        Just s -> do
          liftIO $ forkPingThread c 10
          S.mapM_ (\a -> liftIO $ sendTextData c $ pack . show $ a) (adapt s)
        Nothing -> do
          log $ "NodeId not part of Kibbutz: " <> (show n)
    streamData Nothing _ = log "No NodeId Provided"
