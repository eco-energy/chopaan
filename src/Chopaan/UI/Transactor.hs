{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Chopaan.UI.Transactor where

import Chopaan.UI.Base
import Chopaan.Kibbutz.Kibbutz (Kbtz)
import Chopaan.Kibbutz.Transactor (Tx(..), Stake(..), Role(..))
import Chopaan.Comm.Comm (Address)

import Reflex
import Reflex.Vty
import qualified Data.Text as T

import qualified Data.Map as Map
import Data.Map (Map)
-- What do we want to do in a transactor?
-- Select a bunch of nodes for a transaction by multiselect click 

type Selected n = [(n, Bool)]


transactor :: forall t m n. (UIConstraints t m, Ord n, Show n) => [n] -> VtyWidget t m (Dynamic t (Tx n))
transactor nodes = do
  s <- selections $ (Map.fromList $ (\n -> (n, False)) <$> nodes)
  createTx <- do col $
                   fixed 2 $ textButtonStatic def $ "Create Transaction"
  return $ tx (sequence s)
  where
    tx :: Dynamic t (Map n Bool) -> Dynamic t (Tx n)
    tx = fmap (\st ->
                  Tx
                  $ Map.map (\_ -> (Stake (Source, 0, 0)))
                  $ Map.filter (\x -> x) st)

txForm :: (Reflex t, MonadHold t m) => Event t (Stake) -> m (Dynamic t (Stake))
txForm e = holdDyn (Stake (Source, 0, 0)) e

selections :: forall t m n. (UIConstraints t m, Show n) => Map n Bool
  -> VtyWidget t m (Map n (Dynamic t Bool))
selections nodes = do
  let
    ns :: VtyWidget t m (Map n (Dynamic t Bool))
    ns = col $ do
      sequence $ Map.mapWithKey (\k _ -> stretch $ nodeToggle k) nodes
  ns' <- ns
  return $ ns'



nodeToggle :: (UIConstraints t m, Show n) => n -> VtyWidget t m (Dynamic t Bool)
nodeToggle n = col $ do
  b <- fixed 4 $ textButtonStatic def $ T.pack . show $ n
  toggle False b
