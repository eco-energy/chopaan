{-# LANGUAGE OverloadedStrings, GADTs, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, ConstraintKinds, RankNTypes, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable
, DeriveGeneric
, GeneralizedNewtypeDeriving
, DerivingStrategies
, StandaloneDeriving
, DerivingVia
, DeriveAnyClass
#-}
module Chopaan.Kibbutz.DataSource
  ( KbtzReq(..)
  ) where

import GHC.Generics
import Generics.OneLiner
import Generic.Data

import Control.Monad.Catch
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Except

import Data.Generic.HKD
import Data.Functor.Identity
import Data.Hashable
import Data.Typeable
import qualified Data.Text as T

import qualified Streamly.Prelude as S

import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.HW

import Haxl.Core

type SCon t m = (S.IsStream t, S.MonadAsync m)

type KbtzUID = KbtzId Int 

type NodeUID = NodeId Int

newtype OwnerId = OwnerId Int
  deriving (Eq, Ord, Show, Generic, Bounded, Enum, Hashable)

data Owner = Owner
  { ownerId :: OwnerId
  , epiteth :: T.Text
  } deriving (Eq, Show, Generic, Typeable, Hashable)


data Address = Address
  { unit :: (Maybe T.Text)
  , building :: T.Text
  , street :: T.Text
  , locality :: T.Text
  , city :: T.Text
  , country :: T.Text
  } deriving (Eq, Show, Generic, Typeable, Hashable)

--type Address = Address' Identity

data NodeAttrs = NodeAttrs
  { nodeMAC :: NodeMAC
  , hwConfig :: (HW Double)
  , owner :: Owner
  , address :: (Maybe Address)
  } deriving (Eq, Show, Generic, Typeable)


data KbtzAttrs = KbtzAttrs
  { kbtzName :: KbtzName
  , kbtzChopaan :: Owner
  } deriving (Eq, Show, Generic, Typeable, Hashable)

type NodeAttrsF f = HKD NodeAttrs f

type KbtzAttrsF f = HKD KbtzAttrs f

data KbtzError = InsertionError | DataSourceError
  deriving (Eq, Ord, Show, Generic, Bounded, Enum, Hashable, Exception)

type WithError a = Either KbtzError a

type EIdx i n = WithError (i, n) 


data KbtzReq a where
  GetKbtzim :: forall t m. (SCon t m) => KbtzReq (t m (EIdx KbtzUID KbtzAttrs))
  GetKbtzNodes :: forall t m. (SCon t m) => KbtzName -> KbtzReq (t m (EIdx NodeUID NodeAttrs))
  AddKbtz :: KbtzUID -> KbtzAttrs -> KbtzReq (WithError KbtzUID)
  AddKbtzNode :: KbtzUID -> NodeAttrs -> KbtzReq (WithError NodeUID)
  UpdateKbtz :: KbtzUID -> KbtzAttrs -> KbtzReq (EIdx KbtzUID KbtzAttrs)
  UpdateKbtzNode :: KbtzUID -> NodeUID -> NodeAttrs -> KbtzReq (EIdx NodeUID NodeAttrs)
  RemoveKbtz :: KbtzUID -> KbtzReq (WithError ())
  RemoveKbtzNode :: KbtzUID -> NodeUID -> KbtzReq (WithError ())
  deriving Typeable

deriving instance Eq (KbtzReq a)
deriving instance Show (KbtzReq a)

instance ShowP KbtzReq where showp = show

instance Hashable (KbtzReq a) where
  hashWithSalt s (GetKbtzim) = hashWithSalt s (0::Int, 0::Int)
  hashWithSalt s (GetKbtzNodes kId) = hashWithSalt s (1::Int, kId)
  hashWithSalt s (AddKbtz kId _) = hashWithSalt s (2::Int, kId)
  hashWithSalt s (AddKbtzNode kId nx) = hashWithSalt s (3::Int, kId, (nodeMAC nx))
  hashWithSalt s (UpdateKbtz kId kx) = hashWithSalt s (4::Int, kId, kx)
  hashWithSalt s (UpdateKbtzNode kId nId _) = hashWithSalt s (5::Int, kId, nId)
  hashWithSalt s (RemoveKbtz k) = hashWithSalt s (6::Int, k)
  hashWithSalt s (RemoveKbtzNode k n) = hashWithSalt s (6::Int, k, n)


instance StateKey KbtzReq where
  data State KbtzReq = KReqState


instance DataSourceName KbtzReq where
  dataSourceName _ = "Kbtz"


instance DataSource u KbtzReq where
  fetch = kbtzFetch


kbtzFetch :: State KbtzReq -> Flags -> u -> PerformFetch KbtzReq
kbtzFetch s _flags _user = BackgroundFetch undefined
