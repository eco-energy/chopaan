module Main where

import qualified Data.Text as T
import Chopaan.Hydrate

--import Dhall hiding (newManager, void)
--import Paths_chopaan


-- getOpts = do
--   path <- getDataFileName "hydration.dhall"
--   input auto $ T.pack path


main :: IO ()
main = do
  conf <- parseHConf'
  case conf of
    Left e -> do
      print e
      return ()
    Right c -> runHydration c
