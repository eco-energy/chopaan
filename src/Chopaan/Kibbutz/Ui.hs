{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleInstances, TypeOperators, TypeFamilies, FlexibleContexts, ConstraintKinds, InstanceSigs #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving,  DerivingStrategies, GeneralizedNewtypeDeriving, DeriveAnyClass #-}

module Chopaan.Kibbutz.Ui where

import GHC.Generics

import Control.DeepSeq (NFData)
import Chopaan.Kibbutz.Kibbutz
import Chopaan.Kibbutz.Mesh
import Chopaan.Node.NodeId
import Chopaan.Node.Node
import qualified Shpadoinkle.Html as S
import qualified Shpadoinkle.Widgets.Table as T


newtype EnergyKbtz t m n = EnergyKbtz (Kbtz t m n SensorS)
  deriving (Generic)

newtype MeshKbtz t m n = MeshKbtz (Kbtz t m n MeshNode)
  deriving (Generic)

data instance T.Column (EnergyKbtz t m n) = Column SensorS
data instance T.Row (EnergyKbtz t m n) = Row n

instance (KbtzConn t m n) => T.Tabular (EnergyKbtz t m n) where
  toRows :: EnergyKbtz t m n -> [T.Row (EnergyKbtz t m n)]
  toRows = undefined
  -- toCell :: EnergyKbtz t m n
  --   -> T.Row (EnergyKbtz t m n)
  --   -> T.Column (EnergyKbtz t m n)
  --   -> [S.Html m (EnergyKbtz t m n)] 
  toCell = undefined
  sortTable :: T.SortCol (EnergyKbtz t m n)
    -> T.Row (EnergyKbtz t m n)
    -> T.Row (EnergyKbtz t m n) -> Ordering
  sortTable = undefined
