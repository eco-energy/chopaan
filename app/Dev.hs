module Main where

import qualified Chopaan.Client as C
import qualified Chopaan.Server as S
import           Shpadoinkle.Run (liveWithBackend, Env(Dev))


main :: IO ()
main = liveAndWait 8080 C.app $ S.application Dev "./ui" (S.TinkerConf "localhost" 8182)



liveAndWait p f b = liveWithBackend p f b

-- dev :: IO ()
-- dev = liveWithBackend 8080 app . pure $ S.application $ serveUI @(SPA IO) "." (\r -> do
--    vm <- start r
--    cy <- getCurrentYear
--    return $ template @App Dev vm (view cy vm)) routes
