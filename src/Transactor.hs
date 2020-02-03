{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Transactor where

import Registry (NodeT)
import Node (NodeS)
import qualified Data.Time as Time
import qualified Data.Text as Text
import Data.Word

import Proto.NodeMessages as NM
import Proto.NodeMessages_Fields as NM

import Lens.Micro

import Data.ProtoLens
import Data.Convertible
import Data.Convertible.Instances
import Data.ULID

import GHC.Generics (S, Generic)

--------------------------------------------------------------------------------

runTransactor = undefined


newtype VI a = VI { unVI :: (a, a)} deriving (Eq, Ord, Show, Generic, Functor)

mkVI = VI



transaction = decode . attend . encode
encode :: f g a -> k b
encode = undefined
attend :: k b -> k c
attend = undefined
decode :: k c -> f g b
decode = undefined


----------------------------------------------------------------------------------
-- Energy Transactor

data Transaction = Transaction
  { start :: Time.UTCTime,
    duration   :: Time.DiffTime,
    nodes :: [(NodeT, VI Double)]
  } deriving (Eq, Ord, Show)



mkTxn :: Time.UTCTime -> Time.DiffTime -> [(NodeT, VI Double)] -> Transaction
mkTxn start duration ps  = Transaction start duration ps

{--
zeroTxn :: Time.UTCTime -> Transaction
zeroTxn t0 = Transaction t0 t1 vs
  where
    t1 = (mod minutes (+5) t0)
    vs = map (mkVI . (\a -> (a*0, a*0))) [0..10]
--}

mkETR :: Double -> Int -> NM.PDirection -> Text.Text -> Time.UTCTime -> NM.EnergyTransactionRequest
mkETR power howLong dir uid stime = defMessage
         & uuid .~ uid
         & NM.start .~ (utcToWord64 stime)
         & powerInWatts .~ power
         & durationInSeconds .~ (d' howLong)
         & direction .~ dir
   where
     d' :: Int -> Word64
     d' = convert
     utcToWord64 :: Time.UTCTime -> Word64
     utcToWord64 = c'' . c'
       where
         c' :: Time.UTCTime -> Int
         c' = convert
         c'' :: Int -> Word64
         c'' = convert

{--
transactionRequests :: Transaction -> Time.NominalDiffTime -> IO [NM.EnergyTransactionRequest]
transactionRequests Transaction{..} leadTime = do
  transactionId <- getULID
  now <- Time.getCurrentTime
  let
    startTime = now + leadTime
  return $ map (\(n, vi)-> mkETR (fst vi * snd vi) duration ) nodes



mkRequest :: Double -> Time.DiffTime -> NM.PDirection -> IO NM.EnergyTransactionRequest
mkRequest p t d = do
  ulid <- getULID
  time <- Time.getCurrentTime
  let
    e = mkEtr p time ((Text.pack . show) ulid)
  return etr

--}
