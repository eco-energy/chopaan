{-# LANGUAGE KindSignatures, FlexibleContexts, ScopedTypeVariables, TypeApplications, RankNTypes, FlexibleInstances, ConstraintKinds, InstanceSigs #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE TypeOperators, QuantifiedConstraints, TypeFamilies, CPP #-}
module Chopaan.Kibbutz.Kibbutz (KbtzConn) where

import Prelude hiding (zipWith)

import GHC.Generics

import ChopaanP

import Streamly.Prelude (IsStream, MonadAsync)
import Control.Monad.Bayes.Class (MonadSample)
import Chopaan.Comm.Address (Address)

type KbtzConn t m n = (IsStream t, MonadAsync m, Ord n, Show n, Address n, MonadSample m)

