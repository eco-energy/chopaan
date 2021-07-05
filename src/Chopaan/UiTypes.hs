{-# LANGUAGE KindSignatures, TypeOperators, DataKinds, FlexibleContexts, TypeFamilies, FlexibleInstances, LambdaCase, TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances, InstanceSigs, RecordWildCards, CPP #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving
, DerivingStrategies, DeriveAnyClass, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -dth-dec-file#-}

module Chopaan.UiTypes where

import GHC.Generics

import Control.Lens (makePrisms, makeFieldsNoPrefix, Lens'(..), Prism'(..), prism, lens)
import Control.DeepSeq (NFData)

import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)

import Data.Proxy (Proxy (Proxy))

import Servant.API (Capture, Delete
                   , FromHttpApiData, Get, JSON
                   , Post, Put, QueryParam, Raw
                   , ReqBody, ToHttpApiData
                   , (:<|>) (..), (:>))


import Shpadoinkle.Router (HasRouter ((:>>))
                          , Redirect (Redirect)
                          , Routed (..), View, navigate)
import Shpadoinkle.Widgets.Types (Field, Humanize (..)
                                 , Hygiene (Clean)
                                 , Input (Input, _value)
                                 , Pick (AtleastOne, One)
                                 , Present (present)
                                 , Search (Search)
                                 , Status (Edit, Errors, Valid)
                                 , Validate (rules)
                                 , fullOptions, fullOptionsMin)

import Shpadoinkle.Widgets.Table (SortCol(..))

import Chopaan.Node.NodeId
import Chopaan.Node.NodeT
import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.KbtzimT ()
import Chopaan.CRUD
import Chopaan.Graph
import qualified Data.Time as Ti

type API = "api" :> "kibbutzim" :> Get '[JSON] KbtzList
      :<|> "api" :> "kibbutz" :> Capture "id" KbtzName :> Get '[JSON] NodeList

data GView = GView
  { _whichK :: KbtzName
  , _whichG :: GraphType
  , _startTime :: Ti.UTCTime
  , _endTime :: Ti.UTCTime
  , _currentG :: Maybe (SG NodeMAC)
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

data Frontend = MHomePage
              | MKibbutzim (RosterKbtzim)
              | MKibbutz (RosterNodezim)
              | MGraph GView
              | MAddNode (KbtzName) (Maybe NodeMAC) (NodeUpdate 'Edit)
              deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)


type SPA m = "app" :> View m Frontend
        :<|> "app" :> "kibbutzim" :> View m Frontend
        :<|> "app" :> "kibbutz" :> Capture "id" KbtzName :> View m Frontend
        :<|> "app" :> "kibbutz" :> Capture "id" KbtzName :> "addNode" :> View m Frontend
        :<|> "app" :> "graph" :> Capture "id" KbtzName :> View m Frontend
        :<|> Raw


data Route
  = RHomePage
  | RKibbutzim
  | RGraph KbtzName
  | RKibbutz KbtzName
  | RAddNode KbtzName
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

routes :: SPA m :>> Route
routes =
  RHomePage
  :<|> RKibbutzim
  :<|> RKibbutz
  :<|> RAddNode
  :<|> RGraph
  :<|> RHomePage

instance Routed (SPA m) Route where
  redirect = \case
    RHomePage -> Redirect (Proxy @("app" :> View m Frontend)) id
    RKibbutzim -> Redirect (Proxy @("app" :> "kibbutzim" :> View m Frontend)) id
    RKibbutz k -> Redirect (Proxy @("app" :> "kibbutz" :> Capture "id" KbtzName :> View m Frontend)) ($ k)
    RAddNode k -> Redirect (Proxy @("app" :> "kibbutz" :> Capture "id" KbtzName :> "addNode" :> View m Frontend)) ($ k)
    RGraph k -> Redirect (Proxy @("app" :> "graph" :> Capture "id" KbtzName :> View m Frontend)) ($ k)


{------------------ TABALS -----------}

data RosterKbtzim = RosterKbtzim
  { _sortK :: SortCol KbtzList
  , _searchK :: Input Search
  , _tableK :: KbtzList
  } deriving (Generic, Eq, Ord, Show, NFData, ToJSON, FromJSON)


data RosterNodezim = RosterNodezim
  { _sortN :: SortCol NodeList
  , _searchN :: Input Search
  , _tableN :: NodeList
  } deriving (Generic, Eq, Ord, Show, NFData, ToJSON, FromJSON)



#ifndef ghcjs_HOST_OS
makePrisms ''Frontend

makeFieldsNoPrefix ''GView

makeFieldsNoPrefix ''RosterKbtzim

makeFieldsNoPrefix ''RosterNodezim
#else
-- src/Chopaan/UiTypes.hs:122:1-21: Splicing declarations
_MHomePage :: Prism' Frontend ()
_MHomePage
  = (prism (\ () -> MHomePage))
      (\ x_a1TCA
         -> case x_a1TCA of
              MHomePage -> Right ()
              _ -> Left x_a1TCA)
_MKibbutzim ::
  Prism' Frontend RosterKbtzim
_MKibbutzim
  = (prism
       (\ x1_a1TCB -> MKibbutzim x1_a1TCB))
      (\ x_a1TCC
         -> case x_a1TCC of
              MKibbutzim y1_a1TCD -> Right y1_a1TCD
              _ -> Left x_a1TCC)
_MKibbutz ::
  Prism' Frontend RosterNodezim
_MKibbutz
  = (prism
       (\ x1_a1TCE -> MKibbutz x1_a1TCE))
      (\ x_a1TCF
         -> case x_a1TCF of
              MKibbutz y1_a1TCG -> Right y1_a1TCG
              _ -> Left x_a1TCF)
_MGraph ::
  Prism' Frontend GView
_MGraph
  = (prism
       (\ x1_a1TCH -> MGraph x1_a1TCH))
      (\ x_a1TCI
         -> case x_a1TCI of
              MGraph y1_a1TCJ -> Right y1_a1TCJ
              _ -> Left x_a1TCI)
_MAddNode ::
  Prism' Frontend (KbtzName, Maybe NodeMAC, NodeUpdate  'Edit)
_MAddNode
  = (prism
       (\ (x1_a1TCK, x2_a1TCL, x3_a1TCM)
          -> ((MAddNode x1_a1TCK) x2_a1TCL) x3_a1TCM))
      (\ x_a1TCN
         -> case x_a1TCN of
              MAddNode y1_a1TCO y2_a1TCP y3_a1TCQ
                -> Right (y1_a1TCO, y2_a1TCP, y3_a1TCQ)
              _ -> Left x_a1TCN)
-- src/hs:124:1-26: Splicing declarations
class HasCurrentG s a | s -> a where
  currentG :: Lens' s a
instance HasCurrentG GView (Maybe (SG NodeMAC)) where
  {-# INLINE currentG #-}
  currentG
    f_a1UQ3
    (GView x1_a1UQ4
                           x2_a1UQ5
                           x3_a1UQ6
                           x4_a1UQ7
                           x5_a1UQ8)
    = (fmap
         (\ y1_a1UQ9
            -> ((((GView x1_a1UQ4) x2_a1UQ5) x3_a1UQ6)
                  x4_a1UQ7)
                 y1_a1UQ9))
        (f_a1UQ3 x5_a1UQ8)
class HasEndTime s a | s -> a where
  endTime :: Lens' s a
instance HasEndTime GView Ti.UTCTime where
  {-# INLINE endTime #-}
  endTime
    f_a1UQh
    (GView x1_a1UQi
                           x2_a1UQj
                           x3_a1UQk
                           x4_a1UQl
                           x5_a1UQm)
    = (fmap
         (\ y1_a1UQn
            -> ((((GView x1_a1UQi) x2_a1UQj) x3_a1UQk)
                  y1_a1UQn)
                 x5_a1UQm))
        (f_a1UQh x4_a1UQl)
class HasStartTime s a | s -> a where
  startTime :: Lens' s a
instance HasStartTime GView Ti.UTCTime where
  {-# INLINE startTime #-}
  startTime
    f_a1UQo
    (GView x1_a1UQp
                           x2_a1UQq
                           x3_a1UQr
                           x4_a1UQs
                           x5_a1UQt)
    = (fmap
         (\ y1_a1UQu
            -> ((((GView x1_a1UQp) x2_a1UQq) y1_a1UQu)
                  x4_a1UQs)
                 x5_a1UQt))
        (f_a1UQo x3_a1UQr)
class HasWhichG s a | s -> a where
  whichG :: Lens' s a
instance HasWhichG GView GraphType where
  {-# INLINE whichG #-}
  whichG
    f_a1UQz
    (GView x1_a1UQA
                           x2_a1UQB
                           x3_a1UQC
                           x4_a1UQD
                           x5_a1UQE)
    = (fmap
         (\ y1_a1UQF
            -> ((((GView x1_a1UQA) y1_a1UQF) x3_a1UQC)
                  x4_a1UQD)
                 x5_a1UQE))
        (f_a1UQz x2_a1UQB)
class HasWhichK s a | s -> a where
  whichK :: Lens' s a
instance HasWhichK GView KbtzName where
  {-# INLINE whichK #-}
  whichK
    f_a1UQJ
    (GView x1_a1UQK
                           x2_a1UQL
                           x3_a1UQM
                           x4_a1UQN
                           x5_a1UQO)
    = (fmap
         (\ y1_a1UQP
            -> ((((GView y1_a1UQP) x2_a1UQL) x3_a1UQM)
                  x4_a1UQN)
                 x5_a1UQO))
        (f_a1UQJ x1_a1UQK)
-- src/hs:126:1-33: Splicing declarations
class HasSearchK s a | s -> a where
  searchK :: Lens' s a
instance HasSearchK RosterKbtzim (Input Search) where
  {-# INLINE searchK #-}
  searchK
    f_a1VkG
    (RosterKbtzim x1_a1VkH x2_a1VkI x3_a1VkJ)
    = (fmap
         (\ y1_a1VkK
            -> ((RosterKbtzim x1_a1VkH) y1_a1VkK) x3_a1VkJ))
        (f_a1VkG x2_a1VkI)
class HasSortK s a | s -> a where
  sortK :: Lens' s a
instance HasSortK RosterKbtzim (SortCol KbtzList) where
  {-# INLINE sortK #-}
  sortK
    f_a1VkQ
    (RosterKbtzim x1_a1VkR x2_a1VkS x3_a1VkT)
    = (fmap
         (\ y1_a1VkU
            -> ((RosterKbtzim y1_a1VkU) x2_a1VkS) x3_a1VkT))
        (f_a1VkQ x1_a1VkR)
class HasTableK s a | s -> a where
  tableK :: Lens' s a
instance HasTableK RosterKbtzim KbtzList where
  {-# INLINE tableK #-}
  tableK
    f_a1VkV
    (RosterKbtzim x1_a1VkW x2_a1VkX x3_a1VkY)
    = (fmap
         (\ y1_a1VkZ
            -> ((RosterKbtzim x1_a1VkW) x2_a1VkX) y1_a1VkZ))
        (f_a1VkV x3_a1VkY)
-- src/hs:128:1-34: Splicing declarations
class HasSearchN s a | s -> a where
  searchN :: Lens' s a
instance HasSearchN RosterNodezim (Input Search) where
  {-# INLINE searchN #-}
  searchN
    f_a1VnC
    (RosterNodezim x1_a1VnD x2_a1VnE x3_a1VnF)
    = (fmap
         (\ y1_a1VnG
            -> ((RosterNodezim x1_a1VnD) y1_a1VnG) x3_a1VnF))
        (f_a1VnC x2_a1VnE)
class HasSortN s a | s -> a where
  sortN :: Lens' s a
instance HasSortN RosterNodezim (SortCol NodeList) where
  {-# INLINE sortN #-}
  sortN
    f_a1VnQ
    (RosterNodezim x1_a1VnR x2_a1VnS x3_a1VnT)
    = (fmap
         (\ y1_a1VnU
            -> ((RosterNodezim y1_a1VnU) x2_a1VnS) x3_a1VnT))
        (f_a1VnQ x1_a1VnR)
class HasTableN s a | s -> a where
  tableN :: Lens' s a
instance HasTableN RosterNodezim NodeList where
  {-# INLINE tableN #-}
  tableN
    f_a1VnV

    (RosterNodezim x1_a1VnW x2_a1VnX x3_a1VnY)
    = (fmap
         (\ y1_a1VnZ
            -> ((RosterNodezim x1_a1VnW) x2_a1VnX) y1_a1VnZ))
        (f_a1VnV x3_a1VnY)
#endif
