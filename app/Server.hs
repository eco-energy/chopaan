module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors

import qualified Chopaan.API.History as S



main :: IO ()
main = run 8888 $ (simpleCors $ S.historyApp "localhost" 8182)
