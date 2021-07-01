{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
--{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Chopaan.Server (application) where

import GHC.Generics hiding (R)

import           Control.Monad.Trans.Reader hiding (ask)
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class

import           Data.Proxy

import           Network.Wai               (Application)
import           Network.Wai.Handler.Warp  (run)


import           Servant.API.WebSocket (WebSocket)
import           Servant.Links

import           Servant.API
import           Servant.Server            (Server, hoistServer, serve)

import           Shpadoinkle               (JSM, type (~>))
import           Shpadoinkle.Router        (MonadJSM)
import           Shpadoinkle.Router.Server (serveUI)
import           Shpadoinkle.Run           (Env (Prod))



import Chopaan.UiTypes
import Chopaan.CRUD
import Chopaan.API.History
import Chopaan.View (view, template, onRouteChange)


newtype App a = App { runApp :: ReaderT TinkerConf IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader TinkerConf)


appToHandler :: MonadIO m => TinkerConf -> App ~> m
appToHandler c a = liftIO $ runReaderT (runApp a) c


newtype Noop a = Noop (JSM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadJSM)
  deriving anyclass CRUDChopaan

instance CRUDChopaan App where
  listNodezim k = (\(TinkerConf h p) -> (toHandlerH h p) $ listNodezim k)
                  =<< ask 
  listKibbutzim = (\(TinkerConf h p) -> (toHandlerH h p) listKibbutzim) =<< ask
  getGraph  g k t t' = ask
                       >>= (\(TinkerConf h p) -> toHandlerH h p $ getGraph g k t t')

app :: Env -> FilePath -> TinkerConf -> Application
app ev root (TinkerConf h p) = serve (Proxy @ (SPA App :<|> HistoryAPI)) (serveSPA :<|> (serveHistoryAPI h p))
  where
    serveSPA :: Server (SPA App)
    serveSPA = serveUI @ (SPA App) root
      (\r -> appToHandler (TinkerConf h p) $ do
          i <- onRouteChange r
          return . template ev i $ view @ Noop i) routes


application :: Env -> FilePath -> IO Application
application e f = return $ app e f (TinkerConf "localhost" 8182) 
