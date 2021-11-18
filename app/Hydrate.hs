module Main where

import Control.Monad
import qualified Data.Text as T

import Chopaan.Hydrate
import Chopaan.Graph (tkOptions, runGraphM)
import Chopaan.Types (PoolConf(..))
import Options.Applicative

--import Dhall hiding (newManager, void)
--import Paths_chopaan


-- getOpts = do
--   path <- getDataFileName "hydration.dhall"
--   input auto $ T.pack path


main :: IO ()
main = do
  tc <- execParser tkOptions
  hc <- parseHConf
  runGraphM (PoolConf 1 10 10) tc $ runHydration hc
