module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

import Chopaan.Hydrate
import Chopaan.Graph (tkOptions, runGraphM, getKNs)
import Control.Concurrent.STM (atomically)
import Chopaan.Types (PoolConf(..), icOptions)
import Options.Applicative

--import Dhall hiding (newManager, void)
--import Paths_chopaan


-- getOpts = do
--   path <- getDataFileName "hydration.dhall"
--   input auto $ T.pack path


main :: IO ()
main = do
  tc <- execParser tkOptions
  ic <- execParser icOptions
  hc <- parseHConf
  runGraphM (PoolConf 1 10 10) tc $ runHydration ic hc
    =<< (liftIO . atomically . mkTKbtz =<< getKNs)
