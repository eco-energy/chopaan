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

import           Control.Monad.Trans.Reader
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
--import Chopaan.API.History
import Chopaan.View (view, template, start)



instance HasLink WebSocket where
  type MkLink (WebSocket) r = r 
  toLink toA _ = toA


data Opts = Opts

newtype App a = App { runApp :: ReaderT Opts IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Opts)


toHandler :: MonadIO m => Opts -> App ~> m
toHandler c a = liftIO $ runReaderT (runApp a) c


newtype Noop a = Noop (JSM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadJSM)
  deriving anyclass CRUDChopaan

instance CRUDChopaan App


app :: Env -> FilePath -> Application
app ev root = serve (Proxy @ (API :<|> SPA App)) $ serveAPI :<|> serveSPA
  where
    serveAPI :: Server API
    serveAPI = hoistServer (Proxy @API) (toHandler Opts) $ listKibbutzim
               :<|> listNodezim
               
    serveSPA :: Server (SPA App)
    serveSPA = serveUI @ (SPA App) root
      (\r -> toHandler Opts $ do
          i <- start r
          return . template ev i $ view @ Noop i) routes


application :: Env -> FilePath -> IO Application
application e f = return $ app e f 
