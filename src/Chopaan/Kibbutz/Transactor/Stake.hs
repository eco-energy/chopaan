{-# LANGUAGE NamedFieldPuns, OverloadedStrings, TupleSections #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveFoldable, DeriveTraversable, DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, CPP, BangPatterns #-}

module Chopaan.Kibbutz.Transactor.Stake where

import GHC.Generics (Generic)
import Control.DeepSeq ( NFData )

import Data.Aeson (ToJSON, FromJSON(..))
import qualified Codec.Winery as W
import qualified Data.Time as Time
import Data.Word
import Data.ProtoLens
import Lens.Micro
import Data.Convertible
import Data.Convertible.Instances ()


import Chopaan.Node.Metrics
import qualified Proto.NodeMessageSchema.NodeMessages as NM
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as NM

import Data.Greskell ( lookupAs, Key, FromGraphSON(..) )
import Data.Greskell.GraphSON.GValue (unwrapOne)
import Data.Greskell.Extra ( (<=:>), writeKeyValues )
import NetSpider.Found ( LinkState(..) )
import NetSpider.Graph
    ( EFinds, LinkAttributes(..), NodeAttributes(..), VFoundNode )
import Chopaan.Graph.Greskell ( decodeBin, wineryJSONWrite )

data Role = Source | Sink
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)
  deriving W.Serialise via (W.WineryVariant (Role))

instance FromGraphSON Role where
  parseGraphSON = parseJSON . unwrapOne

newtype Stake' p = Stake
  { unStake :: (Role, p, Time.NominalDiffTime) }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (NFData, ToJSON, FromJSON)
  deriving W.Serialise via (W.WineryRecord (Stake' p))

type Stake = Stake' Watts

stakeKey :: forall n a. Key n a
stakeKey = "txStake"

instance (W.Serialise n) => NodeAttributes (Stake' n) where
  writeNodeAttributes s = fmap writeKeyValues $
    sequence [ (stakeKey @VFoundNode <=:> wineryJSONWrite s)
             ]
  parseNodeAttributes props = (decodeBin "Stake' Node" $ lookupAs stakeKey props)


instance (W.Serialise n) => LinkAttributes (Stake' n) where
  writeLinkAttributes s = fmap writeKeyValues $
    sequence [ (stakeKey @EFinds <=:> wineryJSONWrite s)
             ]
  parseLinkAttributes props = (decodeBin "Stake' Link" $ lookupAs stakeKey props)

roleLinkDir :: Role -> LinkState
roleLinkDir r = case r of
  Source -> LinkToTarget
  Sink -> LinkToSubject
{-# INLINE roleLinkDir #-}

stakeLinkDir :: Stake -> LinkState
stakeLinkDir (Stake (r, _, _)) = roleLinkDir r
{-# INLINE stakeLinkDir #-}


instance (RealFrac p) => Semigroup (Stake' p) where
  (Stake (Source, w, t)) <> (Stake (Source, w', t')) = Stake (Source, w + w', t + t')
  (Stake (Source, w, t)) <> (Stake (Sink, w', t')) = Stake (role, w'', t + t')
    where
      w'' = abs $ w - w'
      role = if w - w' > 0 then Source else Sink
  (Stake (Sink, w, t)) <> (Stake (Source, w', t')) = Stake (role, w'', t + t')
    where
      w'' = abs $ w - w'
      role = if w - w' > 0 then Source else Sink
  (Stake (Sink, w, t)) <> (Stake (Sink, w', t')) = Stake (Sink, abs $ w + w', t + t')

instance (RealFrac p) => Monoid (Stake' p) where
  mempty = Stake (Sink, 0, 0)

mkStake :: Role -> Double -> Int -> Stake
mkStake r p t = Stake (r, toWatts p, fromIntegral t)
{-# INLINE mkStake #-}

stakeToETR :: Stake -> NM.EnergyTransactionRequest
stakeToETR (Stake (role, watts, duration)) = defMessage
                                            & NM.powerInWatts .~ (fromWatts watts)
                                            & NM.durationInSeconds .~ (timeToWord duration)
                                            & NM.direction .~ (toPDir role) 
  where
    toPDir Source = NM.Outgoing
    toPDir Sink = NM.Incoming
    timeToWord :: Time.NominalDiffTime -> Word64
    timeToWord = (convert @Int @Word64) . (round @Time.NominalDiffTime @Int)
{-# INLINE stakeToETR #-}
