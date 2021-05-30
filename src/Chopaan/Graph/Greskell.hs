{-# LANGUAGE ConstraintKinds #-}
module Chopaan.Graph.Greskell where

import Data.Aeson (ToJSON, FromJSON)

import Data.Greskell (FromGraphSON)

type GreskellC a = (ToJSON a, FromJSON a, FromGraphSON a)
