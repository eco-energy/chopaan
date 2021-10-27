module Main where

import qualified Data.Text as T
import Chopaan.Hydrate
import Chopaan.Graph (tkOptions)
import Options.Applicative

--import Dhall hiding (newManager, void)
--import Paths_chopaan


-- getOpts = do
--   path <- getDataFileName "hydration.dhall"
--   input auto $ T.pack path


main :: IO ()
main = do
  tc <- execParser tkOptions
  runHydration tc =<< parseHConf
  -- case conf of
  --   Left e -> do
  --     print e
  --     return ()
  --   Right c -> runHydration c
