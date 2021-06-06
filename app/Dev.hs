module Main where


import Control.Concurrent
import Control.Monad

import qualified Chopaan.Client as C
import qualified Chopaan.Server as S
import qualified Chopaan.View as V
import           Shpadoinkle.Run (Env (Dev), liveWithBackend)


main :: IO ()
main = liveAndWait 8080 C.app $ S.application Dev "./assets"


liveAndWait p f b = liveWithBackend p f b >> forever (threadDelay $ 1000 * 1000)


-- dev :: IO ()
-- dev = liveWithBackend 8080 app . pure $ S.application $ serveUI @(SPA IO) "." (\r -> do
--   vm <- start r
--   cy <- getCurrentYear
--   return $ template @App Dev vm (view cy vm)) routes
