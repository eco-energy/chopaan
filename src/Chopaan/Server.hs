{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuantifiedConstraints, DataKinds, UndecidableInstances #-}
--{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Chopaan.Server (application, main, TinkerConf(..)) where

import GHC.Generics hiding (R)

import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Reader hiding (ask)
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.Catch
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import           Control.Monad.IO.Unlift

import           Data.Proxy

import           Network.Wai               (Application)
import           Network.Wai.Handler.Warp  (run)
import           Network.Wai.Middleware.Cors

import           Streamly.Prelude          (IsStream, AheadT, adapt)
import           Streamly.Internal.Data.Stream.IsStream (hoist)
import qualified Streamly as S

import           Servant.API
import           Servant.Server            (Server, serve)
import           Servant.Server.StaticFiles (serveDirectoryWebApp)

import           Shpadoinkle               (JSM)
import           Shpadoinkle.Router        (MonadJSM)
import           Shpadoinkle.Router.Server (serveUI)
import           Shpadoinkle.Run           (Env (Prod))


import           Options.Applicative       (Parser, ParserInfo, auto,
                                            execParser, fullDesc, header,
                                            helper, info, long, metavar, option,
                                            progDesc, short, showDefault,
                                            strOption, value, (<**>))


import Chopaan.UiTypes
import Chopaan.CRUD
import Chopaan.API.History
import Chopaan.Graph
import Chopaan.View (view, template, onRouteChange)



newtype Noop a = Noop (JSM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadJSM,
                    MonadBase IO, MonadBaseControl IO, MonadThrow)
  deriving anyclass CRUDChopaan


type Static = Raw

app :: Env -> FilePath -> DBPools -> Application
app ev root poo =
  serve (Proxy @ ((HistoryAPI AheadT) :<|> SPA Noop :<|> Static))
  ((serveHistoryAPI poo) :<|> (serveSPA poo) :<|> (serveDirectoryWebApp root))
  where
    serveSPA :: DBPools -> Server (SPA GraphM)
    serveSPA poo = serveUI @ (SPA GraphM) root
      (\r -> runGraphWithDB poo $ do
          i <- onRouteChange r
          return . template ev i $ view @ Noop i) routes


data ServerOpts = ServerOpts
  { assets :: FilePath
  , port :: Int
  , tinkerHost :: String
  , tinkerPort :: Int
  } deriving (Generic)

parser :: Parser ServerOpts
parser = ServerOpts
  <$> strOption   (long "assets" <> short 'a' <> metavar "FILEPATH")
  <*> option auto (long "port"   <> short 'p' <> metavar "PORT" <> showDefault <> value 8080)
  <*> strOption   (long "tinkerHost" <> metavar "TINKERHOST")
  <*> option auto (long "tinkerPort" <> metavar "TINKERPORT" <> showDefault <> value 8182)

options :: ParserInfo ServerOpts
options = info (parser <**> helper) $
    fullDesc <> progDesc "Chopaan Server"
             <> header "Servers the SPA and the Backend API"

application :: Env -> FilePath -> TinkerConf -> IO Application
application e f (TinkerConf h p) = do
  poo <- mkDBPools h p
  return $ simpleCors $ app e f poo 


main :: IO ()
main = do
  ServerOpts{..} <- execParser options
  run port =<< application Prod assets (TinkerConf tinkerHost tinkerPort)
