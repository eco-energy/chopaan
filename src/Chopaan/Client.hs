{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleInstances          #-}

module Chopaan.Client where

import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.Reader        (MonadIO, MonadTrans)
import           Data.Proxy                  (Proxy (..))
#ifndef ghcjs_HOST_OS
import           Shpadoinkle                 (JSM, MonadJSM, MonadUnliftIO (..),
                                              UnliftIO (..), askJSM, runJSM)
#else
import           Shpadoinkle                 (JSM, MonadUnliftIO (..),
                                              UnliftIO (..), askJSM, runJSM)
#endif
import           Servant.API                 ((:<|>) (..))
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html.Utils      (getBody)
import           Shpadoinkle.Router          (fullPageSPA, withHydration)
import           Shpadoinkle.Router.Client   (client, runXHR)

import           Chopaan.CRUD
import           Chopaan.UiTypes              (API, SPA,
                                              routes, Route)
import           Chopaan.View                   (start, view)

newtype AppC a = AppC { runAppC :: JSM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)
#ifndef ghcjs_HOST_OS
  deriving (MonadJSM)
#endif

instance MonadUnliftIO AppC where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = do ctx <- askJSM; return $ UnliftIO $ \(AppC m) -> runJSM m ctx

instance CRUDChopaan AppC

app :: JSM ()
app = fullPageSPA @(SPA JSM) runAppC runParDiff (withHydration start) view getBody start routes
