{-# LANGUAGE NamedFieldPuns, OverloadedStrings, TupleSections #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, CPP #-}
module Chopaan.Kibbutz.Transactor--  ( -- runTransactor
--                                   statePipe
--                                   , Stake(..)
--                                   , Tx(..)
--                                   , TxPlan
--                                   , TxState
--                                   , Role(..)
--                                   , TxStatus(..)
--                                   , foldTxState
--                                   , mkStake
--                                   , dispatchTx
-- --                                  , asKbtz
--                                   , planTx
--                                   --, monitorTx
--                                   , curryTx
--                                   , stakeLinkDir
--                                   , txStatusLinkDir
--                                   , dispatchNodeTx
--                                   ) 
where

import Prelude hiding (zip, zipWith)
import qualified Control.Category as C
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.DeepSeq (NFData)

import Chopaan.Utils.Streamly
import Chopaan.Node.Node (SensorR)
import Chopaan.Node.Metrics (toWattSeconds, toWatts
                            , fromWattSeconds, fromWatts
                            , Watts, WattSeconds
                            , SensorMetrics(..)
                            , Node(..)
                            , pToE
                            , Battery(..)
                            )

import GHC.Generics (Generic)

import qualified Algebra.Graph.Labelled as G
import Algebra.Graph.Labelled (Graph(..))

import qualified Data.Time as Time
import qualified Data.Text as Text
import Data.Word
import Data.Maybe
import Data.Bifunctor
import qualified Proto.NodeMessageSchema.NodeMessages as NM
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as NM

import Lens.Micro

import Data.ProtoLens
import Data.Convertible
import Data.Convertible.Instances ()
import Data.ULID
import Data.Aeson as A
import qualified Data.ByteString.Lazy as BL



import qualified Streamly.Prelude as S
import Streamly (IsStream, MonadAsync, adapt)
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe as P
import qualified Streamly.Internal.Data.Pipe.Types as P

import qualified Data.Map.Strict as M
import Data.Key hiding (Key)
import qualified Data.List as L

import Shpadoinkle.Widgets.Types (Humanize(..))

#ifndef ghcjs_HOST_OS
import Chopaan.Kibbutz.LinOpt
import Chopaan.Comm.Comm (Address(..), PubQueue, writeToPubQ)
import Data.SBV
import ConCat.Misc (R)

import Data.Greskell (lookupAs, Key, pMapToFail, FromGraphSON(..), parseGraphSON, PMapLookupException(..))
import Data.Greskell.Extra (writeKeyValues, (<=:>))
import Data.Greskell.GraphSON.GValue (unwrapOne)
import NetSpider.Found (LinkState(..))
import NetSpider.Graph (LinkAttributes(..), NodeAttributes(..), EFinds, VFoundNode)
import Chopaan.Graph.Greskell
#endif




newtype Tx n a = Tx { unTx :: M.Map n a }
  deriving stock (Eq, Ord, Show, Generic, Traversable)
  deriving newtype (ToJSON, FromJSON, NFData, Functor, Foldable)
  deriving anyclass (Humanize)

instance (Ord n) => Semigroup (Tx n a) where
  (Tx m) <> (Tx m') = Tx (m <> m')

instance (Ord n) => Monoid (Tx n a) where
  mempty = Tx mempty


type TxPlan n = Tx n Stake

type TxState n = Tx n (Role, TxStatus)

type NodeStates n = Tx n SensorR



curryTx :: forall n a. (Ord n) => a -> Tx n a -> n -> a
curryTx defA (Tx p) n = fromMaybe defA $ M.lookup n p
{-# INLINE curryTx #-}

data TxStatus' e = TxStatus'
  { energyDispatched :: e
  , energyReceived :: e
  , energyRemaining :: e
  , lossPerWattSecond :: e
  , totalLoss :: e
  , timeRemaining :: Time.DiffTime
  , startLag :: Time.DiffTime
  , endLag :: Time.DiffTime
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData, Humanize)


type TxStatus = TxStatus' WattSeconds

instance (Ord e, RealFrac e) => Semigroup (TxStatus' e) where
  tx <> tx' = TxStatus'
              { energyDispatched = energyDispatched tx + energyDispatched tx'
              , energyReceived = energyReceived tx + energyReceived tx'
              , timeRemaining = min (timeRemaining tx) (timeRemaining tx')
              , energyRemaining = min (energyRemaining tx) (energyRemaining tx')
              , lossPerWattSecond =  avg (lossPerWattSecond tx) (lossPerWattSecond tx')
              , totalLoss = totalLoss tx + totalLoss tx'
              , startLag = max (startLag tx) (startLag tx')
              , endLag = max (endLag tx) (endLag tx)
              }
              where
                avg a b = (a + b) / 2

instance  (Ord e, RealFrac e) => Monoid (TxStatus' e) where
  mempty = TxStatus'
    { energyDispatched = 0
    , energyReceived = 0
    , timeRemaining = 0
    , energyRemaining = 0
    , lossPerWattSecond = 0
    , totalLoss = 0
    , startLag = 0
    , endLag = 0
    }

#ifndef ghcjs_HOST_OS
planTx :: (MonadAsync m, MonadCatch m,  Ord n, Show n, IsStream t) => Time.DiffTime -> t m (NodeStates n) -> t m (Maybe (TxPlan n))
planTx horizon k = S.postscan (transactionPlanner horizon) k 
{-# INLINE planTx #-}


dispatchTx :: forall m n. (MonadIO m, MonadCatch m, Address n)
  => PubQueue
  -> TxPlan n
  -> m ()
dispatchTx = dispatchNodeTx
{-# INLINE dispatchTx #-}

dispatchNodeTx :: forall m n. (MonadIO m, MonadCatch m, Address n)
  => PubQueue
  -> TxPlan n
  -> m ()
dispatchNodeTx q (Tx tx) = do
  let txDispatches =  (\(nid, st) -> (stateTopic nid, fromStake st)) <$> (M.toList tx)
  sequence_ $ (\(t, s) -> liftIO $ writeToPubQ q t s) <$> txDispatches
{-# INLINE dispatchNodeTx #-}

mkTxDispatch :: (Address n) => Text.Text -> Time.UTCTime -> TxPlan n -> NM.Transaction
mkTxDispatch uid stime (Tx txns) = defMessage
                         & NM.start .~ (utcToWord64 stime)
                         & NM.etrs .~ (M.mapKeys (toRemoteId) $ fromStake <$> txns) 
  where
    utcToWord64 :: Time.UTCTime -> Word64
    utcToWord64 = (convert @Int @Word64) . (convert @Time.UTCTime @Int)



foldTxState :: TxState n -> TxStatus
foldTxState (Tx gt) = let
      gridTx = foldl (<>) mempty $ snd <$> gt
      loss = energyDispatched gridTx - energyReceived gridTx
      lossPerWS = loss / (energyDispatched gridTx)
      in gridTx{totalLoss = loss, lossPerWattSecond = lossPerWS}
{-# INLINE foldTxState #-}
-- The state will just be carried across as a TxStatus

--idFold' :: (Monad m) => FL.Fold m a (Maybe a)
--idFold' = FL.lcatMaybes idFold 

--composeFold :: FL.Fold m a b -> FL.Fold m b c -> FL.Fold m a c
--composeFold f g = g . f
  
idFold :: (Monad m, Monoid a) => FL.Fold m a a
idFold = FL.mkPureId (flip const) mempty
{-# INLINE idFold #-}

secondF :: (Monad m, Monoid a) => FL.Fold m b c -> FL.Fold m (a, b) (a, c)
secondF = FL.unzip idFold
{-# INLINE secondF #-}

firstF :: (Monad m, Monoid c) => FL.Fold m a b -> FL.Fold m (a, c) (b, c)
firstF = (flip FL.unzip) idFold 
{-# INLINE firstF #-}

dupF :: (Monad m, Monoid a) => FL.Fold m a b -> FL.Fold m a (a, b)
dupF f = (,) <$> idFold <*> f  
{-# INLINE dupF #-}

txFold :: forall m n. (MonadIO m, MonadCatch m,  Ord n)
       => TxPlan n
       -> FL.Fold m (NodeStates n, Maybe (TxPlan n)) ((NodeStates n, Maybe (TxPlan n)), TxState n)
txFold = dupF . transactionFold
{-# INLINE txFold #-}


transactionFold :: forall m n. (MonadIO m, MonadCatch m,  Ord n)
                => TxPlan n -> FL.Fold m (NodeStates n, Maybe (TxPlan n)) (TxState n)
transactionFold participants = FL.Fold step start end
  where
    step t n = pure $ incTxState t n
    {-# INLINE step #-}
    start :: m (TxState n)
    start = return $ stakeStatus <$> participants
    {-# INLINE start #-}
    end :: TxState n -> m (TxState n)
    end = pure
    {-# INLINE end #-}
    -- shouldQuit (Tx t) = if (all ((\x -> timeRemaining x <= 0) . snd . snd) (M.toList t))
    --                then ( . Tx $ t)
    --                else ( . Tx $ t)


stakeStatus :: Stake -> (Role, TxStatus)
stakeStatus (Stake (px, w, t)) = (px, mempty{ timeRemaining = t
                                       , energyRemaining = (pToE @Double) (realToFrac t) w
                                       , startLag = 0
                                       })
{-# INLINE stakeStatus #-}

planToState :: TxPlan n -> TxState n
planToState = fmap stakeStatus
{-# INLINE planToState #-}

zipWith3 :: (Ord n) => (a -> b -> c -> d) -> M.Map n a -> M.Map n b -> M.Map n c -> M.Map n d 
zipWith3 f a b c = M.intersectionWith ($) (M.intersectionWith f a b) c


incTxState :: (Ord n) => TxState n -> (NodeStates n, Maybe (TxPlan n)) -> TxState n
incTxState (Tx ts) (Tx ns, plan) = case plan of
  Nothing -> Tx $ zipWith updateTS ts ns
  Just (Tx p) ->
    case (M.size p == 0) of
      True -> Tx $ zipWith updateTS ts ns
      False -> Tx $ zipWith updateTS (fmap stakeStatus p) ns 
  where
    {-# INLINE updateTS #-}
    updateTS :: (Role, TxStatus) -> SensorR -> (Role, TxStatus)
    updateTS (px, prevTx) SensorMetrics{..} = let
      nextTS = case px of
                 Source -> (mempty @TxStatus)
                           { energyDispatched = txEnergy + energyDispatched prevTx
                           , timeRemaining = timeRemaining prevTx - lastTimeDiff
                           , energyRemaining = energyRemaining prevTx - txEnergy
                           , startLag = if hasStarted px
                                        then startLag prevTx
                                        else (startLag prevTx + lastTimeDiff)
                           , endLag = if not shouldHaveEnded
                                      then 0
                                      else (if hasEnded px
                                             then endLag prevTx
                                             else endLag prevTx + lastTimeDiff)
                           }
                 Sink -> (mempty @TxStatus)
                   { energyReceived = txEnergy + energyReceived prevTx
                   , timeRemaining = timeRemaining prevTx - lastTimeDiff
                   , energyRemaining = energyRemaining prevTx - txEnergy
                   , startLag = if hasStarted px
                                then startLag prevTx
                                else (startLag prevTx + lastTimeDiff)
                   , endLag = if not shouldHaveEnded
                              then 0
                              else (if hasEnded px
                                    then endLag prevTx
                                    else endLag prevTx + lastTimeDiff)
                   }
      in (px, nextTS)
      where
        {-# INLINE txEnergy #-}
        txEnergy :: WattSeconds
        txEnergy = (pToE @Double) (realToFrac lastTimeDiff) (tx _powerT)
        {-# INLINE hasStarted #-}
        hasStarted Source = (abs $ tx _powerT) >= eta
        hasStarted Sink = (abs $ tx _powerT) >= eta
        {-# INLINE hasEnded #-}
        hasEnded Source =  shouldHaveEnded && (abs $ tx _powerT) <= eta
        hasEnded Sink = shouldHaveEnded && (abs $ tx _powerT) <= eta
        {-# INLINE shouldHaveEnded#-}
        shouldHaveEnded = (timeRemaining prevTx) <= 0
        {-# INLINE eta #-}
        eta = 0.5
{-# INLINE incTxState #-}
    
-- transactor :: forall m n. P.Pipe m (NodeStates n) (TxPlan n, TxState)
-- transactor = P.Pipe consumer producer i
--   where
--     i = undefined
--     consumer :: TxState n -> (NodeStates n) -> m (P.Step (P.PipeState x TxState n)) (TxPlan, TxState)
--     consumer p n = pure $ P.Yield () 
--     producer :: y -> m (P.Step (P.PipeState TxPlan y) (TxPlan, TxState))
--     producer = undefined


-- planPipe :: forall m n. (MonadIO m, MonadCatch m,  Ord n)
--   => Time.DiffTime -> P.Pipe m (NodeStates n) (Maybe (TxPlan n))
-- planPipe = P.mapM . txn

-- planToStatus :: forall m n. (MonadIO m, MonadCatch m,  Ord n)
--              => P.Pipe m (Maybe (TxPlan n)) (Maybe (TxPlan n, TxState n)) 
-- planToStatus = P.zipWith (\x y -> (,) <$> x <*> y) C.id (P.map (fmap planToState))

-- ntos :: (MonadIO m, MonadCatch m,  Ord n) => Time.DiffTime ->  P.Pipe m (NodeStates n) (Maybe (TxPlan n, TxState n))
-- ntos t = planToStatus C.. (planPipe t)

-- statePipe :: (MonadIO m, MonadCatch m,  Ord n) => Time.DiffTime -> P.Pipe m (NodeStates n) (Maybe (NodeStates n, TxPlan n, TxState n))
-- statePipe t = (P.zipWith status (ntos t) (P.map Just))

-- statePipeWithId h = P.zipWith (,) (P.map fst) (P.compose (statePipe h) (P.map snd))

-- statusPipe :: forall m n. (MonadIO m, MonadCatch m,  Ord n)
--   => P.Pipe m (TxState n) (NodeStates n -> TxState n)
-- statusPipe = P.map incTxState

--stateP = P.zipWith statusPipe 
--applyInPipe = 

-- status :: ( Ord n)
--   => Maybe (TxPlan n, TxState n)
--   -> Maybe (NodeStates n)
--   -> Maybe (NodeStates n, TxPlan n, TxState n)
-- status x y = (,,) <$> y <*> (fst <$> x) <*> (liftA2 incTxState (snd <$> x) y)

-- statusS :: (IsStream t, MonadAsync m,  Ord n)
--         => t m (n, NodeStates n, TxPlan n)
--         -> t m (n, NodeStates n, TxPlan n, TxState n)
-- statusS = fmap status


transactionPlanner :: forall m n. (MonadIO m, MonadCatch m, Show n, Ord n) => Time.DiffTime -> FL.Fold m (NodeStates n) (Maybe (TxPlan n))
transactionPlanner timeHorizon = FL.Fold (\_ n -> txn timeHorizon n) (pure mempty) pure
{-# INLINE transactionPlanner#-}


txn' h t = (pure . (fromMaybe mempty)) =<< txn h t
{-# INLINE txn' #-}


txn :: forall m n. (MonadIO m, MonadCatch m, Ord n, Show n) => Time.DiffTime -> NodeStates n -> m (TxPlan' n)
txn h (Tx ns) = do
  -- liftIO . print $ (better mkSources sources)
  -- liftIO . print $ (better mkSinks sinks)
  -- liftIO . print $ d
  let nodes = M.keys ns
  let indexer = M.fromList $ zip [1..] nodes
      getAtI i = indexer M.! i
      reindexTx (Tx n) = Tx $ M.fromList $
                         fmap (\(i, a) -> (getAtI i, a)) $ M.toList n
  sched <- schedule
  --liftIO . print $ sched
  return $ fmap reindexTx sched
      where
        consumption = M.toAscList $ fmap _demand ns
        {-# INLINE consumption #-}
        storage = M.toAscList $
                  fmap (\n ->
                          (totalCapacity . _battery $ n) * (soc . _battery $ n))
                  ns
        {-# INLINE storage #-}
        d = fmap (\(i, (c, s))
                     -> (i, c - s)) $ zip [1..] $ zip (snd <$> consumption) (snd <$> storage)
        {-# INLINE d #-}
        (sources, sinks) = L.partition (\x -> snd x > 0) d
        {-# INLINE better #-}
        better f ss = uncurry f $ unzip $ (second fromWattSeconds) <$> ss
        {-# INLINE schedule #-}
        schedule :: m (TxPlan' Int)
        schedule =  (fmap join) . tryForMaybe $ (solveTP h)
                    (better mkSources sources)
                    (better mkSinks sinks)
          [[1 -- (fromIntegral $ mod j 2) * 1000
           | i <- [1..length sources]] | j <- [1..length sinks]]
{-# INLINE txn #-}

tryForMaybe :: (MonadIO m, MonadCatch m) => m a -> m (Maybe a) 
tryForMaybe m = expToMaybe =<< (try m)
{-# INLINE tryForMaybe #-}

expToMaybe :: (MonadIO m) => Either SomeException a -> m (Maybe a)
expToMaybe (Left e) = (liftIO . print $ e) >> return Nothing
expToMaybe (Right a) = return $ Just a
{-# INLINE expToMaybe #-}


type TxPlan' n = Maybe (TxPlan n)


solveTP :: forall m . (MonadIO m, MonadCatch m) => Time.DiffTime -> Sources Int -> Sinks Int -> [[Double]] -> m (TxPlan' Int)
solveTP timeHorizon sources sinks cs = do
  liftIO $ do
    (LexicographicResult sol) <- optimize Lexicographic $ transportProblem sources sinks cs
    -- let
    --   pSol (Unsatisfiable _ x) = print "Unsatisfiable"-- >> print x
    --   pSol (Satisfiable _ m) = print "Satisfiable!"-- >> print m
    --   pSol (SatExtField _ m) = print "Satisfies Extension Field Only!" >> print m
    --   pSol (Unknown _ s) = print "Unknown!" >> print s
    --   pSol (ProofError _ s _) = print "Proof Error!" >> print s
      
      
    -- liftIO . pSol $ sol
    let dict = getModelDictionary sol
    --liftIO . print $ "Plan:\n" <> (show dict)
    if not . modelExists $ sol then return Nothing else do
      let (ns, cvs) = unzip $ M.toAscList dict
      --liftIO . print $ dict
      case ((M.lookup "goal" dict)) of
        Nothing -> return Nothing
        Just x -> do
          let
            vs' :: M.Map String Double
            vs' = M.fromAscList $ zip ns (parseToDoubles cvs)
            toTransferMat :: M.Map String Double -> [[Double]]
            toTransferMat m = (zipWith (zipWith (+))) ((fmap (fmap (* (-1)))) . L.transpose $ x') x'
              where
                x' = [[zeroIfNone $ M.lookup (tName i j) m | i <- getNames sources] | j <- getNames sinks]
            transferMat = toTransferMat vs'
            sourceTransmit = (toWattSeconds . abs) <$> (fmap sum $ L.transpose transferMat)
            sinkReceive = (toWattSeconds . abs) <$> (fmap sum $ transferMat)
            asSources = toSourceStake timeHorizon <$> (zip (getNames sources) sourceTransmit)
            asSinks = toSinkStake timeHorizon <$> (zip (getNames sinks) sinkReceive)
            planDict = M.fromList $ (asSources) <> (asSinks)
          --print ("Plan Dict: " <> show planDict)
          return . Just . Tx $ planDict
    where
      isZeroStake (_, (Stake (_, a, t))) = a > 0 && t > 0 
      zeroIfNone Nothing = 0
      zeroIfNone (Just a) = a
      parseToDoubles ys = case (parseCVs @Double) ys of
        Just (a, rs) -> (a:parseToDoubles rs)
        Nothing -> []
      toSourceStake t (i, e) = (i, Stake (Source, (e2p t e), t))
      toSinkStake t (i, e) = (i, Stake (Sink, (- e2p t e), t))
      e2p :: Time.DiffTime -> WattSeconds -> Watts
      e2p t ws = toWatts $ (fromWattSeconds ws) / (realToFrac t)
{-# INLINE solveTP #-}
#endif


{---
    Concretely
----}

data Role = Source | Sink
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

#ifndef ghcjs_HOST_OS
instance FromGraphSON Role where
  parseGraphSON = parseJSON . unwrapOne
#endif

newtype Stake' p = Stake
  { unStake :: (Role, p, Time.DiffTime) }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (NFData, ToJSON, FromJSON)
  deriving anyclass (Humanize)

type Stake = Stake' Watts

#ifndef ghcjs_HOST_OS
stakeKey :: forall n. Key n BL.ByteString
stakeKey = "txStake"

instance (ToJSON n, FromJSON n) => NodeAttributes (Stake' n) where
  writeNodeAttributes s = fmap writeKeyValues $
    sequence [ (stakeKey @VFoundNode <=:> A.encode s)
             ]
  parseNodeAttributes props = pMapToFail (decodeBin $ lookupAs stakeKey props)


decodeBin (Left a) = (Left a)
decodeBin (Right x) = case A.decode x of
        Nothing -> (Left $
                    PMapParseError "Transactor or Stake Key" "aeson decode failed for sensor metrics")
        Just x' -> Right x'

instance (ToJSON n, FromJSON n) => LinkAttributes (Stake' n) where
  writeLinkAttributes s = fmap writeKeyValues $
    sequence [ (stakeKey @EFinds <=:> A.encode s)
             ]
  parseLinkAttributes props = pMapToFail (decodeBin $ lookupAs stakeKey props)



roleLinkDir :: Role -> LinkState
roleLinkDir r = case r of
  Source -> LinkToTarget
  Sink -> LinkToSubject

stakeLinkDir :: Stake -> LinkState
stakeLinkDir (Stake (r, _, _)) = roleLinkDir r
{-# INLINE stakeLinkDir #-}

txStatusLinkDir :: TxStatus -> LinkState
--txStatusLinkDir = const LinkBidirectional
txStatusLinkDir TxStatus'{energyDispatched, energyReceived} = if energyDispatched > 0 && energyDispatched == 0
  then LinkToTarget
  else if energyReceived > 0 && energyDispatched == 0
       then LinkToSubject
       else LinkBidirectional
{-# INLINE txStatusLinkDir #-}
#endif

instance (RealFrac p) => Semigroup (Stake' p) where
  (Stake (Source, w, t)) <> (Stake (Source, w', t')) = Stake (Source, w + w', t + t')
  (Stake (Source, w, t)) <> (Stake (Sink, w', t')) = Stake (role, w'', t + t')
    where
      w'' = abs $ w - w'
      role = if w - w' > 0 then Source else Sink
  (Stake (Sink, w, t)) <> (Stake (Source, w', t')) = Stake (role, w'', t + t')
    where
      w'' = abs $ w - w'
      role = if w - w' > 0 then Source else Sink
  (Stake (Sink, w, t)) <> (Stake (Sink, w', t')) = Stake (Sink, abs $ w + w', t + t')

instance (RealFrac p) => Monoid (Stake' p) where
  mempty = Stake (Sink, 0, 0)

mkStake :: Role -> Double -> Int -> Stake
mkStake r p t = Stake (r, toWatts p, fromIntegral t)


fromStake :: Stake -> NM.EnergyTransactionRequest
fromStake (Stake (role, watts, duration)) = defMessage
                                            & NM.powerInWatts .~ (fromWatts watts)
                                            & NM.durationInSeconds .~ (timeToWord duration)
                                            & NM.direction .~ (toPDir role) 
  where
    toPDir Source = NM.Outgoing
    toPDir Sink = NM.Incoming
    timeToWord :: Time.DiffTime -> Word64
    timeToWord = (convert @Int @Word64) . (round @Time.DiffTime @Int)


#ifndef ghcjs_HOST_OS
txStatusKey :: forall n. Key n BL.ByteString
txStatusKey = "txStatusKey"


instance (ToJSON n, FromJSON n) => LinkAttributes (TxStatus' n) where
  writeLinkAttributes s = fmap writeKeyValues $
    sequence [ (txStatusKey @EFinds <=:> A.encode s)
             ]
  parseLinkAttributes props = pMapToFail (decodeBin $ lookupAs txStatusKey props)

instance (ToJSON n, FromJSON n) => NodeAttributes (TxStatus' n) where
  writeNodeAttributes s = fmap writeKeyValues $
    sequence [ (txStatusKey @VFoundNode <=:> A.encode s)
             ]
  parseNodeAttributes props = pMapToFail (decodeBin $ lookupAs txStatusKey props)
#endif
