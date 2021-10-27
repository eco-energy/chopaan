{-# LANGUAGE TypeApplications, MultiParamTypeClasses, FlexibleInstances #-}
module Chopaan.Kibbutz.LinOpt where

import Data.SBV
import Data.List


newtype Sources n = Sources { unSource :: [(n, Double)] } deriving (Eq, Ord, Show)

newtype Sinks n = Sinks { unSink :: [(n, Double)] } deriving (Eq, Ord, Show)

type NSources = Sources String
type NSinks = Sinks String

instance Semigroup (Sources n) where
  (Sources a) <> (Sources b) = Sources (a <> b)

instance Semigroup (Sinks n) where
  (Sinks a) <> (Sinks b) = Sinks (a <> b)

instance Monoid (Sources n) where
  mempty = Sources []

instance Monoid (Sinks n) where
  mempty = Sinks []
  
class NamedF a n where
  getVals :: a n -> [Double]
  getNames :: a n -> [n]

instance (Show n) => NamedF Sources n where
  getVals = (snd <$>) . unSource
  {-# INLINE getVals #-}
  getNames = (fst <$>) . unSource
  {-# INLINE getNames #-}
  
instance (Show n) => NamedF Sinks n where
  getVals = (snd <$>) . unSink
  {-# INLINE getVals #-}
  getNames = (fst <$>) . unSink
  {-# INLINE getNames #-}

mkSources :: Show n => [n] -> [Double] -> Sources n
mkSources ns vs = Sources $ zip ns vs
{-# INLINE mkSources #-}

mkSinks :: Show n => [n] -> [Double] -> Sinks n
mkSinks ns vs = Sinks $ zip ns vs
{-# INLINE mkSinks #-}

transportProblem :: Show n => Sources n -> Sinks n -> [[Double]] -> Goal
transportProblem ss ds cs = do
  vars <- txVars
  mapM_ (\(xs, t) -> constrain $ sum xs .>= t) $ zip vars (fromDouble <$> (getVals ds))
  mapM_ (\(xs, t) -> constrain $ sum xs .<= t) $ zip (transpose vars) (fromDouble <$> (getVals ss))
  minimize "goal" $ sum $ (fmap sum) $ hadmard vars (fmap (fmap fromDouble) cs)
  where
    txVars :: Symbolic [[SReal]]
    txVars = sequence . (fmap sequence) $ [[sReal $ tName i j
                                           |i <- getNames ss]
                                          | j <- getNames ds]
    fromDouble :: Double -> SReal
    fromDouble = realToFrac
    hadmard :: (Num a) => [[a]] -> [[a]] -> [[a]]
    hadmard as bs = fmap (\(xs, ys) -> fmap (\(x, y) -> x * y) $ zip xs ys) $ zip as bs
{-# INLINE transportProblem #-}


tName :: Show a => a -> a -> String
tName i j = ("x_" <> (show i) <> "_" <> (show j))
{-# INLINE tName #-}

fromName :: String -> (String, String)
fromName ('x':'_':next) = let
  f = takeWhile (\x -> x /= '_') next
  s = drop (length f + 1) next
  in (f, s)
fromName (_) = error "This Should ONLY Be Called for a tName"
{-# INLINE fromName #-}
