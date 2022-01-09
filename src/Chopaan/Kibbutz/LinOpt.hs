{-# LANGUAGE TypeApplications, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, DeriveAnyClass, DerivingStrategies, DerivingVia, DeriveGeneric, DeriveFunctor, ExplicitForAll, ScopedTypeVariables, TupleSections, ConstraintKinds, AllowAmbiguousTypes #-}
module Chopaan.Kibbutz.LinOpt (solveTP, HasVarName(..)) where

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
  deriving (Fractional, Floating, Mergeable, EqSymbolic, OrdSymbolic) via (SReal)

instance Eq SumSym where
 a == b = isConcretely ((getSumSym a) .== (getSumSym b)) (== True) 

getSumSym = getSum . getSym
mkSumSym :: (Real a) => a -> SumSym
mkSumSym = SumSym . pure . realToFrac

type TPScalar a = (Num a, Real a, Fractional a, Floating a)
type TPCon f a = (Functor f, Foldable f, Zip f, Applicative f, TPScalar a)

-- $ The total inflow at a node must exceed the demand there
constrainDemand :: (TPCon f a) => f SumSym -> a -> Goal 
constrainDemand nodeIncomings nodeDemand = do
  assertWithPenalty "demandConstraint" dc (Penalty 0.5 $ Just "demandGroup")
  constrain $ tIn .<= d
    where
      dc = tIn .>= d
      tIn = getSumSym $ sum nodeIncomings
      d = realToFrac $ nodeDemand

-- $ The total outflow at a node must be less than its spare capacity 
constrainSupply :: (TPCon f a) => f SumSym -> a -> Goal 
constrainSupply nodeOutgoings nodeSpareCapacity = constrain c
  -- assertWithPenalty "supplyConstraint" c (Penalty 0.5 (Just "supplyGroup"))
  where
    c = (getSumSym $ sum nodeOutgoings) .<= (realToFrac nodeSpareCapacity)

isPos :: SumSym -> Goal
isPos x = constrain $ x .>= 0  

iAt60v :: Fractional a => a -> a
iAt60v p =  p / 60

lossAt60v :: Fractional a => a -> a -> a
lossAt60v p d = ((i * i) * (distanceToResistance d)) * 1000
  where
    i = iAt60v p
    
distanceToResistance :: (Num a, Fractional a) => a -> a
distanceToResistance d = d * resistivity / crossSection
  where
    resistivity = 1.724e-8
    crossSection = 4e-3
    
powerBalance :: (Foldable f, Functor f) => f SumSym -> Goal
powerBalance = constrain . (.== 0) . abs . getSumSym . sum

txLoss xs ys = sum $ fmap abs (Data.Key.zipWith lossAt60v xs ys)  

dxLoss xs ys = abs $ xs <.> ys

transportCost' :: (TPCon f a, Num b, Floating b)
  => (Distance a -> b) -> f (Distance a) -> f b -> b
transportCost' toB dist tx = txLoss tx (toB <$> dist) -- + (sum $ d ^-^ tx)

transportCost :: (TPCon f a, Floating a) => f (Distance a) -> f SumSym -> SReal
transportCost dx tx = getSumSym $ transportCost' getL dx tx -- (fmap (mkSumSym . unNT) d)
  where
    getL = SumSym . Sum . getD
    getDemand = SumSym . Sum . realToFrac
    unNT (Source a) = a
    unNT (Sink a) = a
    unNT (Passive a) = a

getD :: (Real b, Floating b, Fractional a) => Distance b -> a
getD = realToFrac . fromMaybe infinity . getFinite . getDistance

data NodeType a = Source a | Sink a | Passive a
  deriving (Eq, Ord, Show, Generic, Functor)
  --deriving (Num, Fractional, Floating, Real, RealFrac) via (a)

isSource (Source _) = True
isSource _ = False
isSink (Sink _) = False
isSink _ = False

toNodeType :: (Eq a, Ord a, Num a) => a -> NodeType a
toNodeType a
  | a == 0 = Passive 0
  | a > 0 = Source a
  | a < 0 = Sink (abs a)
  | otherwise = Passive a
  
type TxG n = AG.Graph SumSym n

type DistanceG n a = AG.Graph (Distance a) n

type BipartiteTx n a = G.Graph (n, NodeType a)

constrainNode :: forall n a. (TPScalar a, Ord n) => TxG n -> (n, NodeType a) -> Goal
constrainNode g (n, Source a) = constrainSupply (getOutputs g n) a
constrainNode g (n, Sink a) = constrainDemand (getInputs g n) a
constrainNode g (n, Passive a) = pure () -- sequence_ $ fmap constrainZero ((getInputs g n) <> (getOutputs g n))
  where
    constrainZero x = constrain $ x .== 0
    
transportProblem :: forall n a. (HasVarName n, Ord n, Num a, Real a, Floating a)
  => String -> DistanceG n a -> TxG n -> BipartiteTx n a -> Goal
transportProblem costName g gSym dx = do
  let
    allVars = fmap ex $ AG.edgeList gSym
  sequence_ $ fmap (constrainNode gSym) dx
  sequence_ $ fmap isPos allVars
  minimize costName $ transportCost (ex <$> AG.edgeList g) (ex <$> AG.edgeList gSym) -- (G.vertexList $ fmap snd dx)
  where
    exS (l, _, _) = l
    ex (l, _, _) = l

newtype TPKey = TPKey Int
  deriving (Eq, Ord)
  deriving newtype (Num, Enum, Bounded, Integral, Real, Show, Read)

instance HasVarName TPKey where
  getVarName = show
  fromVarName = read

-- sumEdges :: forall n a. (Ord n, Ord a, Num a) => AG.Graph a n -> G.Graph (n, a)
-- sumEdges = AG.foldg G.empty (G.vertex . (, 0)) newG
--   where
--     newG :: a -> G.Graph (n, a) -> G.Graph (n, a) -> G.Graph (n, a)
--     newG l g@(G.Vertex (n, a)) g'@(G.Vertex (n', a')) = case (compare l 0) of
--       EQ -> G.connect g g'
--       GT -> G.connect (G.Vertex (n, a + l)) g'
--       LT -> G.connect g (G.Vertex (n', a' + l))
      
exampleTP :: DistanceG TPKey Double
          -> (DistanceG TPKey Double -> G.Graph (TPKey, Double))
          -> IO (AG.Graph Double TPKey)
exampleTP g dx = do
  ex <- solveTP 10 g (dx g)
  return $ ex
  

pathD = G.path . fmap (\x -> if even x then (x, (600 :: Double)) else (x, (-500))) . AG.vertexList

spokeD :: DistanceG TPKey Double -> G.Graph (TPKey, Double) 
spokeD = fmap w . G.edges . fmap (\(_, n, n') -> (n, n')) . AG.edgeList
  where
    w n
      | mod n 10 == 0 = (n, 1000)
      | otherwise = let (TPKey i) = n in (n, (- 25 * (realToFrac (i)))) 
    
spokeG' :: Distance Double -> TPKey -> TPKey -> DistanceG TPKey Double
spokeG' d start n = AG.edges $ fmap ((d, start, )) [(start + 1)..n]

spokeG :: DistanceG TPKey Double
spokeG = spokeG' 10 0 9

bigSpokeG :: DistanceG TPKey Double
bigSpokeG = foldl (AG.connect 100) AG.empty
  [
  --AG.overlay
    --((AG.vertex s) (AG.vertex (s + 10)))
    (spokeG' (toD i) s (s + 9))
  | i <- [10, 20..30], s <- [0, 10..30]]
  where
    toD = distance . fromMaybe 0 . finite

pathG :: DistanceG TPKey Double
pathG = AG.edges $ (uncurry toE) <$> (Prelude.zip [0, 1..3] [1, 2..4])
  where
    toE i j = (dis i j, toKey i, toKey j)
    toKey = TPKey . round
    dis i j = distance . fromMaybe 0 . finite $ 5 * (1 + (realToFrac $ mod (round i) 5)) 

solveTP :: forall m n a.
  (MonadIO m, MonadFail m, HasVarName n, Ord n, Num a, Real a, Fractional a, SymVal a, Floating a)
  => NominalDiffTime
  -> DistanceG n a
  -> G.Graph (n, a)
  -> m (AG.Graph a n)
solveTP timeHorizon gSingle dx' = do
  let dx = fmap (second toNodeType) dx'
      g = AG.transitiveClosure gSingle
      costName = "transactionCost"
  (LexicographicResult sol) <- liftIO $
    (\a -> print a >> return a)
    =<< (optimize Lexicographic $
         (flip (transportProblem costName g) dx) =<< (txGraph g))
  return $ parseSol g sol

parseSol :: forall n a. (Ord n, Ord a, Fractional a, HasVarName n)
  => AG.Graph (Distance a) n -> SMTResult -> AG.Graph a n
parseSol g s = g''
  where
    dict = getModelDictionary s
    mkE (_, n, n') = (((Sum . fromRational . toRational . (fromCV @AlgReal)) <$> M.lookup (tName n n') dict), n, n')
    g'' :: AG.Graph a n
    g'' = first (getSum . fromMaybe mempty) $ AG.edges
      $ fmap mkE
      $ AG.edgeList
      $ g

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
      l' <- (sReal name) -- observe name <$> 
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
