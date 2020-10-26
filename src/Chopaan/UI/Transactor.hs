{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Chopaan.UI.Transactor (transactor) where

import Chopaan.UI.Base
import Chopaan.Kibbutz.Transactor (Tx(..), TransactionStatus)

import Reflex
import Reflex.Vty
import qualified Data.Text as T


transactor :: forall t m n. (UIConstraints t m, Ord n, Show n)
  => Dynamic t (Tx n)
  -> Dynamic t (TransactionStatus)
  -> VtyWidget t m ()
transactor txs txStatuses = do
  let txs' = toText <$> txs
      txStatuses' = toText <$> txStatuses
  col $ do
    stretch $ richText def $ current txs'
    stretch $ richText def $ current txStatuses'


toText :: (Show a) => a -> T.Text
toText = T.pack . show
