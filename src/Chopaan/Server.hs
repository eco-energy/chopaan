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
{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}
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


import           Data.Proxy

import           Network.Wai               (Application)
import           Network.Wai.Handler.Warp  (run)
import           Network.Wai.Middleware.Cors

import           Streamly                  (IsStream, AheadT, adapt)
import           Streamly.Internal.Prelude (hoist)
import qualified Streamly as S

import           Servant.API.WebSocket (WebSocket)
import           Servant.Links

import           Servant.API
import           Servant.Server            (Server, hoistServer, serve)
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


newtype App a = App { runApp :: ReaderT TinkerConf IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader TinkerConf,
                    MonadBase IO, MonadBaseControl IO, MonadThrow, MonadCatch)


appToHandler :: MonadIO m => TinkerConf -> App ~> m
appToHandler c a = liftIO $ runReaderT (runApp a) c


newtype Noop a = Noop (JSM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadJSM)
  deriving anyclass CRUDChopaan

-- (forall t. IsStream t => (Monad (t App))) =>
instance CRUDChopaan App where
  listNodezim k = (\(TinkerConf h p) -> (runGraphM h p) $ listNodezim k)
                  =<< ask 
  listKibbutzim = (\(TinkerConf h p) -> (runGraphM h p) listKibbutzim) =<< ask
  getGraph g k t t' = S.aheadly $ do
    (TinkerConf h p) <- ask
    g <- runGraphM h p $ do
      return $ getGraph g k t t'
    hoistS h p $ g


app :: Env -> FilePath -> TinkerConf -> Application
app ev root (TinkerConf h p) =
  serve (Proxy @ ((HistoryAPI AheadT) :<|> SPA App :<|> Raw))
  ((serveHistoryAPI h p) :<|> serveSPA :<|> (serveDirectoryWebApp root))
  where
    serveSPA :: Server (SPA App)
    serveSPA = serveUI @ (SPA App) root
      (\r -> appToHandler (TinkerConf h p) $ do
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
application e f tk = return . simpleCors $ app e f tk 


main :: IO ()
main = do
  ServerOpts{..} <- execParser options
  run port =<< application Prod assets (TinkerConf tinkerHost tinkerPort)
