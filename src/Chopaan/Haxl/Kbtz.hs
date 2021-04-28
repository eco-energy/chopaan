{-# LANGUAGE OverloadedStrings, StandaloneDeriving, RecordWildCards,
    GADTs, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable,
    FlexibleInstances, OverloadedStrings
#-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor #-}

module Chopaan.Haxl.Kbtz where

import GHC.Generics

import qualified Data.Text as Text
import Data.Greskell
import Data.Hashable
import Data.Typeable
import Data.Function ((&))
import Data.Aeson (ToJSON, FromJSON)

import NetSpider.Graph (NodeAttributes(..), VFoundNode(..))
import Haxl.Core
import Control.Monad

import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.HW


data ChopaanReq a where
  GetAllKbtz :: ChopaanReq [KbtzName]
  GetKbtzNodes :: KbtzName -> ChopaanReq [NodeMAC]
  GetHWConfigById :: NodeMAC -> ChopaanReq (HW Double)
  deriving (Typeable)

instance Hashable (ChopaanReq a) where
  hashWithSalt s (GetKbtzNodes k) = hashWithSalt s (0 :: Int, k)
  hashWithSalt s (GetHWConfigById a) = hashWithSalt s (1 :: Int, a)

deriving instance Show (ChopaanReq a)
instance ShowP ChopaanReq where showp = show

instance StateKey ChopaanReq where
  data State ChopaanReq = KbtzState {}


instance DataSourceName ChopaanReq where
  dataSourceName _ = "KbtzDataSource"


instance DataSource u ChopaanReq where
  fetch _state _flags _userEnv = SyncFetch $ \blockedFetches -> do
    let
      allIdVars :: [ResultVar [NodeMAC]]
      allIdVars = [r | BlockedFetch (GetKbtzNodes k) r <- blockedFetches]

      idStrings :: [String]
      idStrings = map show ids

      ids :: [NodeMAC]
      vars :: [ResultVar (HW Double)]
      (ids, vars) = unzip
      
        [(nodeId, r) | BlockedFetch (GetHWConfigById nodeId) r <- blockedFetches]

    unless (null allIdVars) $ do
      allIds <- sql "select id from ids"
      mapM_ (\r -> putSuccess r allIds) allIdVars



sql = undefined




