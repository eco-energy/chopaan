{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, FlexibleContexts, ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE ConstraintKinds #-}

module Chopaan.Comm.Socket where -- (subN, pubN)

import Data.Function ((&))

import Control.Monad.Catch

import Streamly
-- import Streamly.Internal.Data.Array.Storable.Foreign (Array)
-- import qualified Streamly.Prelude as S
-- import qualified Streamly.Network.Socket as SK
-- import Streamly.Network.Socket (SockSpec(..))
-- import Streamly.Internal.Network.Socket (handleWithM, handleWith)


-- import Network.Socket
-- import Unsafe.Coerce
-- import Data.Word

-- import Chopaan.Comm.Comm (Dispatch(..), Address(..))
-- import Chopaan.Node.NodeId (NodeMAC)

-- eth_p_all :: ProtocolNumber
-- eth_p_all = 0x0003


-- defSpec :: SockSpec
-- defSpec = SockSpec
--        { sockFamily = AF_PACKET
--        , sockType = Raw
--        , sockProto = unsafeCoerce . getAddrInfo . unsafeCoerce $ eth_p_all 
--        , sockOpts = []
--        }

-- defAddr = SockAddrUnix "what" --8090 (tupleToHostAddress (0,0,0,0))

-- type ChannelConf = (SockSpec, SockAddr)

-- type NodeConf = (NodeMAC, ChannelConf)

-- type SocketTM t m = (IsStream t, MonadAsync m, MonadMask m)

-- sub' :: forall t m. SocketTM t m
--   => ChannelConf -> t m (Array Word8)
-- sub' (spec, addr) =
--   S.concatMap handleSocketRead (S.unfold SK.accept (maxListenQ, spec, addr))
--   where
--     handleSocketRead :: Socket -> t m (Array Word8)
--     handleSocketRead = flip handleWith (S.unfold SK.readChunks)
--     maxListenQ = 10


-- decode :: (Address n, Dispatch a) => Array Word8 -> (n, a) 
-- decode = undefined

-- sub1 :: forall t m a. (SocketTM t m, Dispatch a) => NodeConf -> t m a
-- sub1 (n, c) = fmap snd $ S.filter (\(n', _) -> n' == n) $ decode <$> (sub' c)

-- pub1 :: forall t m n a. (SocketTM t m, Address n, Dispatch a) => n -> t m a -> t m (Maybe Bool)
-- pub1 = undefined

-- subN :: (SocketTM t m, Dispatch a) => [NodeConf] -> t m (t m a)
-- subN = (fmap sub1) . S.fromList

-- pubN :: forall t m n a. (SocketTM t m, Address n, Dispatch a) => [n] -> t m (t m a) -> t m (t m (Maybe Bool))
-- pubN n xs = pub1 <$> S.fromList n <*> xs
