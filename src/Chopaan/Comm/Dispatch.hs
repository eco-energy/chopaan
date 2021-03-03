{-# LANGUAGE FlexibleContexts #-}

module Chopaan.Comm.Dispatch where

import Lens.Micro hiding (_Just)
import Proto.NodeMessageSchema.NodeMessages hiding (Outgoing, Incoming)
import Proto.NodeMessageSchema.NodeMessages_Fields

import Data.ProtoLens
import Data.ProtoLens.Prism

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

parseDispatch :: (Message a) => BL.ByteString -> Either String a
parseDispatch = decodeMessage . BS.concat . BL.toChunks

class (Show a) => Dispatch a where
  frame :: a -> MeshFrame
  unframe :: MeshFrame -> Maybe a


instance Dispatch EnergyState where
  frame es = defMessage & maybe'payload .~ (_Just # _MeshFrame'State # es) 
  unframe = accessEnergyState

instance Dispatch RuntimeStats where
  frame es = defMessage & maybe'payload .~ (_Just # _MeshFrame'RtStats # es) 
  unframe = accessRTS

instance Dispatch EnergyTransactionRequest where
  frame etr = defMessage & maybe'payload .~ (_Just # _MeshFrame'NodeTxRequest # etr) 
  unframe = accessETR

instance Dispatch HardwareConfig where
  frame hwc = defMessage & maybe'payload .~ (_Just # _MeshFrame'Hw # hwc)
  unframe = accessHWConf

instance Dispatch NodeControl where
  frame nc = defMessage & maybe'payload .~ (_Just # _MeshFrame'Control # nc)
  unframe = accessNodeControl

instance Dispatch Transaction where
  frame tx = defMessage & maybe'payload .~ (_Just # _MeshFrame'Transaction # tx) 
  unframe = accessTx


{---------------------------- Utils -------------------------------------}

fromPayload m l = m ^? maybe'payload . _Just . l

accessEnergyState :: MeshFrame -> Maybe EnergyState
accessEnergyState m = fromPayload m _MeshFrame'State
                      
accessETR :: MeshFrame -> Maybe EnergyTransactionRequest
accessETR m = fromPayload m _MeshFrame'NodeTxRequest

accessRTS :: MeshFrame -> Maybe RuntimeStats
accessRTS m = fromPayload m _MeshFrame'RtStats

accessHWConf :: MeshFrame -> Maybe HardwareConfig
accessHWConf m = fromPayload m _MeshFrame'Hw

accessNodeControl :: MeshFrame -> Maybe NodeControl
accessNodeControl m = fromPayload m _MeshFrame'Control

accessTx :: MeshFrame -> Maybe Transaction
accessTx m = fromPayload m _MeshFrame'Transaction
