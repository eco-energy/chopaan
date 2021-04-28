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



import Chopaan.API.History
import Chopaan.UiTypes
import Chopaan.CRUD


application :: Env -> IO Application
application = undefined

instance HasLink WebSocket where
  type MkLink (WebSocket) r = r 
  toLink toA _ = toA


data Opts = Opts

newtype App a = App { runApp :: ReaderT Opts IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Opts)


toHandler :: MonadIO m => Opts -> app ~> m
toHandler c a = liftIO $ runReaderT (runApp a) c


newtype Noop a = Noop (JSM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadJSM)
  deriving anyclass CRUDChopaan
