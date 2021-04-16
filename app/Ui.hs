module Main where


import qualified Chopaan.Client as C
import           Shpadoinkle.Run (runJSorWarp)


main :: IO ()
main = runJSorWarp 8080 C.app
