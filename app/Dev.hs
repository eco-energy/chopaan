module Main where

--import Chopaan as C
import           Shpadoinkle.Run (runJSorWarp)


main :: IO ()
main = undefined

  -- liveAndWait 8080 C.app $ S.application Dev "./assets"


--liveAndWait p f b = liveWithBackend p f b >> forever (threadDelay $ 1000 * 1000)


-- dev :: IO ()
-- dev = liveWithBackend 8080 app . pure $ S.application $ serveUI @(SPA IO) "." (\r -> do
--   vm <- start r
--   cy <- getCurrentYear
--   return $ template @App Dev vm (view cy vm)) routes
