{-# LANGUAGE DeriveGeneric #-}

-- | Inlined from eco-energy/lcirc (module LCirc.Cospan).
-- A cospan i -> apex <- o (the open-system gluing structure for LCirc).
module LCirc.Cospan
  ( Cospan(..)
  ) where

import GHC.Generics (Generic)

data Cospan a i o = Cospan
  { apex  :: a
  , legIn :: i
  , legOut :: o
  } deriving (Eq, Show, Generic)
