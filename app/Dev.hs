module Main where

import qualified Chopaan.Client as C
import qualified Chopaan.Server as S
import qualified Chopaan.View as V
import           Shpadoinkle.Run (Env (Dev), liveWithBackend)


main :: IO ()
main = C.main --liveWithBackend 8080 C.app $ S.application Dev "./static"
