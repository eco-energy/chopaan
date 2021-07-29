{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ExplicitForAll, TypeApplications, TypeOperators, PackageImports, DeriveGeneric, DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# OPTIONS_GHC -dsuppress-idinfo #-}
{-# OPTIONS_GHC -dsuppress-uniques #-}
{-# OPTIONS_GHC -dsuppress-module-prefixes #-}

-- {-# OPTIONS_GHC -ddump-simpl #-}

-- {-# OPTIONS_GHC -ddump-rule-rewrites #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=25 #-}  -- default 100
-- {-# OPTIONS_GHC -fsimpl-tick-factor=250 #-}  -- default 100

{-# OPTIONS -fplugin-opt=ConCat.Plugin:showResiduals #-}

--{-# OPTIONS -fplugin-opt=ConCat.Plugin:showCcc #-}

{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}

module Main where

import qualified Chopaan.Client as C
import           Shpadoinkle.Run (runJSorWarp, live)
import System.Environment
import System.Envy
import GHC.Generics

data ClientArgs = ClientArgs
  { serverHost :: String
  , serverPort :: Int
  } deriving (Show, Generic, FromEnv)


defCA = ClientArgs "localhost" 8080

main :: IO ()
main = do
  c <- decodeWithDefaults defCA
  runJSorWarp 8080 (C.app (serverHost c) (serverPort c))
