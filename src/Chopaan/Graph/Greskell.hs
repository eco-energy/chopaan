{-# LANGUAGE ConstraintKinds #-}
module Chopaan.Graph.Greskell where

import Data.Text hiding (toLower)
import Data.Aeson (ToJSON, FromJSON, parseJSON)
import qualified Data.Aeson as Aeson

import Data.Char (toLower)


import Data.Greskell
import Data.Greskell.GraphSON.GValue (unwrapOne, unwrapAll)

type GreskellC a = (ToJSON a, FromJSON a, FromGraphSON a)


parseUnwrapTraversable :: (Traversable t, FromJSON (t GValue), FromGraphSON a)
                        => GValue -> Parser (t a)
parseUnwrapTraversable gv = traverse parseGraphSON =<< (parseJSON $ unwrapOne gv)


optSumEncoding :: String -> String -> Aeson.Options
optSumEncoding tag contents  =
  Aeson.defaultOptions
  { Aeson.constructorTagModifier = tagMod,
    Aeson.sumEncoding =
      Aeson.ObjectWithSingleField
      -- { Aeson.tagFieldName = tag,
      --   Aeson.contentsFieldName = contents
      -- }
  }
  where
    tagMod = id
