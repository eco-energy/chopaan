{-# LANGUAGE KindSignatures, TypeOperators, DataKinds, FlexibleContexts, TypeFamilies, FlexibleInstances, LambdaCase, TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, UndecidableInstances, InstanceSigs, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving
, DerivingStrategies, DeriveAnyClass, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Chopaan.Kibbutz.KbtzimT where


import GHC.Generics
import Control.Monad.Identity (Identity)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Control.Lens (makeFieldsNoPrefix)
import Database.Beam (Beamable, Columnar, Nullable) --, Table (..), TableEntity)



import Shpadoinkle.Widgets.Types (Humanize (..))
                                 --, Field, Hygiene (Clean))

import Chopaan.Kibbutz.KbtzId



data KbtzimT f = Kbtzim
  { _kbtzId   :: Columnar f (KbtzId Int)
  , _kbtzName :: Columnar f (KbtzName)
  , _kbtzDesc :: Columnar (Nullable f) Text
  } deriving (Generic, Beamable)

instance NFData (KbtzimT Identity)
deriving instance Eq (KbtzimT Identity)
deriving instance Ord (KbtzimT Identity)
deriving instance Show (KbtzimT Identity)
deriving instance ToJSON (KbtzimT Identity)
deriving instance FromJSON (KbtzimT Identity)
deriving instance Humanize (KbtzimT Identity)

type Kbtzim = KbtzimT Identity

makeFieldsNoPrefix ''KbtzimT
