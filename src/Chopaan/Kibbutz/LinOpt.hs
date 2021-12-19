{-# LANGUAGE TypeApplications, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, DeriveAnyClass, DerivingStrategies, DerivingVia, DeriveGeneric, DeriveFunctor, ExplicitForAll, ScopedTypeVariables #-}
module Chopaan.Kibbutz.LinOpt where

import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Catch

import ConCat.Free.VectorSpace
import Data.SBV
import qualified Data.SBV.Trans as SBVT (optimize)
import Data.SBV.Control
import Data.Time (NominalDiffTime)
import Data.Bifunctor
import Data.Key
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Chopaan.Graph.Algebraic as AG
import qualified Algebra.Graph as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import Algebra.Graph.Label (Distance(..), Capacity(..)
                           , NonNegative, finite, distance, getFinite, getDistance, getCapacity)
import qualified Numeric.Units.Dimensional.Prelude as D
import Data.Monoid
--class 

-- $ In order to solve a transportation problem,
-- $ we must first divide our nodes into sources and sinks,
-- $ then constrain the sum of all outputs each source to be less than the excess capacity there,
-- $ likewise constrain the total input of each sink to be at least the projected demand,
-- $ and then find the least costly transfer where each transaction from source_i to sink_j
-- $ has a cost equivalent to the power multipled by the distance between i and j.


class HasVarName a where
  getVarName :: a -> String
  fromVarName :: String -> Maybe a


newtype SumSym = SumSym { getSym :: Sum SReal }
  deriving (Generic)
  deriving newtype (Semigroup, Monoid, Num)

instance Eq SumSym where
 a == b = isConcretely ((getSumSym a) .== (getSumSym b)) (== True) 

getSumSym = getSum . getSym

-- $ The total inflow at a node must exceed the demand there
constrainDemand :: (Foldable f, Functor f, Num a, Real a) => Int -> f SumSym -> a -> Goal 
constrainDemand i nodeIncomings nodeDemand = do
  constrain ((observe ("incoming_" <> (show i)) $ getSumSym $ sum nodeIncomings) .>= (observe ("demand_" <> (show i)) $ realToFrac $ nodeDemand))
  -- assertWithPenalty "demandConstraint" (Penalty 1.05 Nothing)
  --assertWithPenalty "excessDemand" ((abs . getSumSym $ sum nodeIncomings) .<= (realToFrac . abs $ nodeDemand)) (Penalty 2.0 Nothing)

-- $ The total outflow at a node must be less than its spare capacity 
constrainSupply :: (Foldable f, Functor f, Num a, Real a) => Int -> f SumSym -> a -> Goal 
constrainSupply i nodeOutgoings nodeSpareCapacity = constrain $
  (observe ("outgoing_" <> (show i)) $ getSumSym $ sum nodeOutgoings) .<= (observe ("spareCap_" <> (show i)) $ realToFrac nodeSpareCapacity)

powerBalance :: (Foldable f, Functor f) => f SumSym -> Goal
powerBalance = constrain . (.<= 10) . abs . getSumSym . sum

transportCost' :: (Functor f, Zip f, Foldable f, Num a, Real a, Num b)
  => (Distance a -> b) -> f (Distance a) -> f b -> f b -> b
transportCost' toB distance tx demand = sum $ abs <$> (demand ^-^ tx) -- ((toB <$> distance) <.> tx) +  

-- $ This cost is fucked. We actually should have these SumSyms in the edges as well :(
transportCost :: (Functor f, Zip f, Foldable f, Num a, Real a) => f (Distance a) -> f SumSym -> f a -> (SReal)
transportCost dx tx demand = observe "txCost" . getSumSym $ (transportCost' getL dx tx (getDemand <$> demand))
  where
    getL = SumSym . Sum . realToFrac . fromMaybe 1 . getFinite . getDistance
    getDemand = SumSym . Sum . realToFrac

transportProblem :: forall n a. (HasVarName n, Ord n, Num a, Real a) => String -> AG.Graph (Distance a) n -> AG.Graph SumSym n -> G.Graph (n, a) -> Goal
transportProblem costName g gSym dx = do
  let sources = G.vertexList $ G.induce ((> 0) . snd) dx
      sinks = G.vertexList $ G.induce ((< 0) . snd) dx
  sequenceA $ fmap (\(i, (n, a)) -> constrainSupply i (getOutputs gSym n) a) $ keyed sources
  sequenceA $ fmap (\(i, (n, a)) -> constrainDemand i (getInputs gSym n) a) $ keyed sinks
  powerBalance $ fmap ex $ AG.edgeList gSym
  minimize costName $ transportCost (ex <$> AG.edgeList g) (ex <$> AG.edgeList gSym) (G.vertexList $ fmap snd dx)
  where
    exS (l, _, _) = l
    ex (l, _, _) = l

newtype TPKey = TPKey Int
  deriving (Eq, Ord)
  deriving newtype (Num, Enum, Bounded, Integral, Real, Show, Read)

instance HasVarName TPKey where
  getVarName = show
  fromVarName = read

exampleTP :: AG.Graph (Distance Double) TPKey
          -> G.Graph (TPKey, Double)
          -> IO (AG.Graph (Sum Double) TPKey)
exampleTP = solveTP 10 

dx = G.path $ fmap (\x -> if even x then (x, (600 :: Double)) else (x, (-500))) $ AG.vertexList pathG

pathG :: AG.Graph (Distance Double) TPKey
pathG = AG.edges $ (uncurry toE) <$> (Prelude.zip [0, 1..100] [1, 2..101])
  where
    toE i j = (dis i j, toKey i, toKey j)
    toKey = TPKey . round
    dis i j = distance . fromMaybe 10 . finite $ 5 * (realToFrac $ mod (round i) 5) 

solveTP :: forall m n a. (MonadIO m, MonadFail m, HasVarName n, Ord n, Num a, Real a, SymVal a)
  => NominalDiffTime
  -> AG.Graph (Distance a) n
  -> G.Graph (n, a)
  -> m (AG.Graph (Sum a) n)
solveTP timeHorizon g dx = do
  let costName = "transactionCost"
  (LexicographicResult sol) <- liftIO $ (\a -> print a >> return a) =<< (optimize Lexicographic $
    (flip (transportProblem costName g) dx) =<< (txGraph g))
  let
    goalCost = M.lookup costName dict
    dict = getModelDictionary sol
    mkE (n, n') = (((Sum . fromCV) <$> M.lookup (tName n n') dict), n, n')
    g'' :: AG.Graph (Maybe (Sum a)) n
    g'' = AG.edges
      $ fmap mkE
      $ G.edgeList . fmap fst
      $ dx
  return $ first (fromMaybe mempty) g''

getInputs :: forall n. (Ord n)
  => AG.Graph SumSym n -> n -> [SumSym]
getInputs g' n = fmap fst $ fromMaybe [] $
                     AG.inputs <$> (AG.context ((== n)) g')

getOutputs :: forall n. (Ord n)
  => AG.Graph SumSym n -> n -> [SumSym]
getOutputs g' n = fmap fst $ fromMaybe [] $
                     AG.outputs <$> (AG.context ((== n)) g')

txGraph :: forall n a. (HasVarName n, Ord n, Num a, Real a)
  => AG.Graph (Distance a) n -> Symbolic (AG.Graph SumSym n)
txGraph = (fmap AG.edges) . (sequenceA . fmap edgeSym) . AG.edgeList
  where
    edgeSym (l, n, n') = do
      let name = (tName n n')
      l' <- observe name <$> (sReal name)
      return (SumSym $ Sum l', n, n')

--vertexPairVar :: (Show v) => AG.Graph e v -> G.Graph (String, v)
--vertexPairVar = G.edges . fmap (\(l, x, y) -> tName ) . AG.edgeList

tName :: HasVarName a => a -> a -> String
tName i j = ("x_" <> (getVarName i) <> "_" <> (getVarName j))
{-# INLINE tName #-}

fromName :: String -> (String, String)
fromName ('x':'_':next) = let
  f = takeWhile (\x -> x /= '_') next
  s = drop (length f + 1) next
  in (f, s)
fromName (_) = error "This Should ONLY Be Called for a tName"
{-# INLINE fromName #-}
