{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Chopaan.Run
import RIO
import RIO.Process
import qualified RIO.Text as T
import Dhall
import Chopaan.Types
import Paths_chopaan

main :: IO ()
main = do
  options <- getOptions
  lo <- logOptionsHandle stderr (logVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run

getOptions :: IO (Options)
getOptions = do
  optsPath <- getDataFileName "options.dhall"
  caCert <- getDataFileName "certs/ca.cert"
  cert <- getDataFileName "certs/chopaan.cert.pem"
  key <- getDataFileName "certs/chopaan.private.key.pem"
  fileOptions@Options{mqttOpts} <- input auto $ T.pack optsPath
  return $ fileOptions{
        mqttOpts=mqttOpts{ certPath = cert
                         , keyPath = key
                         , caPath = caCert }
        }
