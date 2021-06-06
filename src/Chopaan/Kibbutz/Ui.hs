{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleInstances, TypeOperators, TypeFamilies, FlexibleContexts, ConstraintKinds, InstanceSigs #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving,  DerivingStrategies, GeneralizedNewtypeDeriving, DeriveAnyClass, QuantifiedConstraints #-}

module Chopaan.Kibbutz.Ui where

import GHC.Generics

import Control.DeepSeq (NFData)
import Chopaan.Kibbutz.Kibbutz

import Shpadoinkle
import qualified Shpadoinkle.Html as H
import qualified Shpadoinkle.Widgets.Table as T
import qualified Shpadoinkle.Widgets.Types as T
import qualified Data.Map.Strict as M
import qualified Data.Text as Txt
import Data.Aeson

type TblConn n a = (Show a, Enum n, T.Humanize a)

newtype TblMap n a = TblMap { runTbl :: M.Map n a }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data instance T.Column (TblMap n a) = Column n
data instance T.Row (TblMap n a) = Row a


showTbl :: (T.Humanize a) => a -> Txt.Text
showTbl = T.humanize



instance (forall m. Monad m, TblConn n a) => T.Tabular (TblMap n a) where
  type Effect (TblMap n a) m = (MonadJSM m)
  toRows :: TblMap n a -> [T.Row (TblMap n a)]
  toRows = undefined
  toCell :: TblMap n a
                  -> T.Row (TblMap n a)
                  -> T.Column (TblMap n a)
                  -> [H.Html m (TblMap n a)] 
  toCell _ (Row a) (Column c) = [H.text . showTbl $ a] 
  sortTable :: T.SortCol (TblMap n a)
    -> T.Row (TblMap n a)
    -> T.Row (TblMap n a) -> Ordering
  sortTable = undefined
