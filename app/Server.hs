module Main where

import qualified Chopaan.Server as S
import Paths_chopaan


main :: IO ()
main = S.main =<< (getDataFileName "options.dhall")
