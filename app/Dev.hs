module Main where

import qualified Chopaan.Client as C
import qualified Chopaan.Server as S
import           Shpadoinkle.Run (liveWithBackend, Env(Dev))
import Control.Monad
import Control.Concurrent

import Paths_chopaan


main :: IO ()
main = do
  opts <- getDataFileName "options.dhall"
  liveAndWait 8080 (C.frontend "localhost" 8080)
    $ S.application opts Dev "./webdev" (S.TinkerConf "localhost" 8182)



liveAndWait p f b = liveWithBackend p f b  >> forever (threadDelay $ 1000 * 1000)

-- dev :: IO ()
-- dev = liveWithBackend 8080 app . pure $ S.application $ serveUI @(SPA IO) "." (\r -> do
--    vm <- start r
--    cy <- getCurrentYear
--    return $ template @App Dev vm (view cy vm)) routes
