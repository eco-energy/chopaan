{-# LANGUAGE RecordWildCards #-}
module Transactor where

import Registry (NodeId(..))
import EnergyState (Watts)
import qualified Data.Time as Time
import qualified Data.Text as Text
import Data.Word


import GHC.Generics (S, Generic)

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
    nodes :: [(NodeId, VI Double)]
  } deriving (Eq, Ord, Show)


mkTxn :: (Num a, Num p) => Time.UTCTime -> a -> [VI p] -> Transaction
mkTxn start duration ps  = Transaction start duration ps

zeroTxn :: Time.UTCTime -> Transaction
zeroTxn t0 = Transaction t0 t1 vs
  where
    t1 = (modL minutes (+5) t0)
    vs = map (mkVI . (\a -> (a*0, a*0))) [0..10]

mkETR :: Double -> Time.DiffTime -> NM.PDirection -> Text.Text -> Time.UTCTime -> NM.EnergyTransactionRequest
mkETR power howLong d uid start = defMessage
         & uuid .~ uid
         & start .~ (utcToWord64 start)
         & powerInWatts .~ power
         & durationInSeconds .~ (sToW64 howLong)
         & direction .~ d
   where
     sToW64 :: S -> Word64
     sToW64 = convert
     utcToWord64 :: Time.UTCTime -> Word64
     utcToWord64 = c'' . c'
       where
         c' :: Time.UTCTime -> Int
         c' = convert
         c'' :: Int -> Word64
         c'' = convert

transactionRequests :: Transaction -> Time.DiffTime -> IO [NM.EnergyTransactionRequest]
transactionRequests Transaction{..} leadTime = do
  transactionId <- getULID
  time <- Time.getCurrentTime + leadTime
  return $ map (\(n, vi)-> mkETR (fst vi * snd vi) duration ) nodes



mkRequest :: Transaction -> Watts -> Time.DiffTime -> NM.PDirection -> IO NM.EnergyTransactionRequest
mkRequest p t d = do
  ulid <- getULID
  time <- Time.getCurrentTime
  let
    e = mkEtr (Text.pack . show) ulid
  return etr

