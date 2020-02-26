{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Transactor where

import Registry (PubQueue)
import Node (NodeId(..))
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

data Transaction' a = Transaction'
  { start :: Time.UTCTime,
    duration   :: Time.DiffTime,
    nodes :: [(NodeId a, VI Double)]
  } deriving (Eq, Ord, Show)



mkTxn' :: Time.UTCTime -> Time.DiffTime -> [(NodeId a, VI Double)] -> Transaction'
mkTxn' start duration ps  = Transaction' start duration ps

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

newtype Transaction a = Transaction { stakes :: [(NodeId a, Double)] } deriving (Eq, Ord, Show, Generic)

data TransactorS a = TransactorS
  { nodes_t :: [NodeId a]
  , transactions :: [Transaction a]
  , txForms :: [Stake a]
  } deriving (Generic)


data Stake a = Stake
  { _stakingNode :: NodeId a
  , _participating :: Bool
  , _power :: Double
  , _duration :: Int
  } deriving (Eq, Ord, Show)


energyStake :: Stake a -> Double
energyStake Stake {..} = _power * (fromIntegral _duration)

validateStakeListForTx :: [Stake a] -> Bool
validateStakeListForTx ss = energyBalance == 0 && powerBalance == 0
  where
    energyBalance = sum $ map energyStake ss
    powerBalance = sum $ map _power ss

toTransaction :: [Stake a] -> Transaction a
toTransaction ss = Transaction $ map (\s-> (_stakingNode s, energyStake s)) ss

prepTx :: [Stake a] -> Time.NominalDiffTime -> IO ([(NodeId a, NM.EnergyTransactionRequest)], Transaction a)
prepTx sf leadTime = do
  txId <- (Text.pack . show) <$> getULID
  startTime <- Time.addUTCTime leadTime <$> Time.getCurrentTime
  let
    txReqs = map (\(n, et) -> (n, et txId startTime)) etrs
    tx = toTransaction stakes
  return (txReqs, tx)
  where
    etrs = map toETR stakes
    stakes = filter (_participating) sf
    toETR :: Stake -> (NodeId, (Text.Text -> Time.UTCTime -> NM.EnergyTransactionRequest))
    toETR Stake {..} = (_stakingNode, msg)
      where
        msg = mkETR (abs _power) _duration dir
        dir = if (_power > 0) then NM.Outgoing else NM.Incoming

executeTransaction :: TransactorS a -> PubQueue -> IO (TransactorS a)
executeTransaction t@TransactorS{..} outQueue = if validateStakeListForTx txForms then exec else return t
  where
    exec = do
      (reqs, tx) <- prepTx txForms (60 * 2 :: Time.NominalDiffTime)
      _ <- (mapM (uncurry $ writeToPubQ outQueue) reqs)
      return $ mkTransactor nodes_t $ tx:transactions

initStake :: NodeId a -> Stake a
initStake n = Stake n False 0 0
