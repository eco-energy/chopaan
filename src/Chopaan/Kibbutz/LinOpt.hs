module Chopaan.Kibbutz.LinOpt where


import Data.SBV
import qualified Data.Map.Strict as M

runTP :: Goal -> IO (SMTResult)
runTP g = do
  (LexicographicResult c) <- optimize Lexicographic g
  return c

r :: Goal -> IO ()
r g = (print . getModelDictionary) =<< runTP g

solveTP :: [Double] -> [Double] -> [[Double]] -> IO [[Double]]
solveTP ss ds cs = do
  (LexicographicResult sol) <- optimize Lexicographic $ transportProblem ss ds cs
  let dict = getModelDictionary sol
  return [] {--[[dict M.! tName i j
          |(_, i) <- zip ss [1..]]
         | (_, j) <- zip ds [1..]]--}

transportProblem :: [Double] -> [Double] -> [[Double]] -> Goal
transportProblem ss ds cs = do
  vars <- txVars
  mapM_ (\(xs, t) -> constrain $ sum xs .>= t) $ zip vars (fromDouble <$> ds)
  mapM_ (\(xs, t) -> constrain $ sum xs .<= t) $ zip (transpose vars) (fromDouble <$> ss)
  minimize "goal" $ sum $ (fmap sum) $ hadmard vars (fmap (fmap fromDouble) cs)
  where
    transpose :: [[a]] -> [[a]]
    transpose = sequence
    txVars :: Symbolic [[SReal]]
    txVars = sequence . (fmap sequence) $ [[sReal $ tName i j
                                           |(_, i) <- zip ss [1..]]
                                          | (_, j) <- zip ds [1..]]
      where
        tName i j = ("x_" <> (show i) <> "_" <> (show j))
    --constrainSumTo :: [SReal] -> SReal -> Goal
    --constrainSumTo xs t = do
      
    fromDouble :: Double -> SReal
    fromDouble = realToFrac
    hadmard :: (Num a) => [[a]] -> [[a]] -> [[a]]
    hadmard as bs = fmap (\(xs, ys) -> fmap (\(x, y) -> x * y) $ zip xs ys) $ zip as bs

tName i j = ("x_" <> (show i) <> "_" <> (show j))
