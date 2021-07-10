{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, TemplateHaskell  #-}

module Chopaan.Client where

import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.Reader        (MonadIO)
import           Data.Proxy                  (Proxy (..))
#ifndef ghcjs_HOST_OS
import           Shpadoinkle                 (JSM, MonadJSM, MonadUnliftIO (..),
                                              UnliftIO (..), askJSM, runJSM)
#else
import           Shpadoinkle                 (JSM, MonadUnliftIO (..),
                                              UnliftIO (..), askJSM, runJSM)
#endif

import           Data.FileEmbed              (embedFile)
import           Data.Text.Encoding          (decodeUtf8)

import           Servant.API                 ((:<|>) (..))
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html.Utils      (getBody, addInlineStyle)
import           Shpadoinkle.Router          (fullPageSPA, withHydration)
import           Shpadoinkle.Router.Client   (client, runXHR', runXHR)
import           Servant.Client.JS

import           Chopaan.CRUD
import           Chopaan.UiTypes              (API, SPA,
                                              routes, Route(..), Frontend(..))
import           Chopaan.API.History
import           Chopaan.View                   (ainit, ginitM, onRouteChange, view, template)

import           Shpadoinkle.Run             (runJSorWarp, Env(Dev))

newtype AppC a = AppC { runAppC :: JSM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)
#ifndef ghcjs_HOST_OS
  deriving (MonadJSM)
#endif

instance MonadUnliftIO AppC where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = do
    ctx <- askJSM
    return $ UnliftIO $ \(AppC m) -> runJSM m ctx

instance CRUDChopaan AppC where
  listKibbutzim = AppC $ runXHR listKibbutzimM
  listNodezim = AppC . runXHR . listNodezimM
  getGraph k g t0 t1 = AppC $ do
    let
      r = historyAPI k g t0 t1
      env = (ClientEnv $ BaseUrl Http devHost 8080 "")
    runXHR' r env

prodHost = "dosti.ecoenergy.global"
devHost = "localhost"
devEnv = ClientEnv $ BaseUrl Http devHost 8080 ""
prodEnv = ClientEnv $ BaseUrl Https prodHost 443 ""

(listKibbutzimM :<|> listNodezimM)
  = client (Proxy @ API)

(historyAPI)
  = client (Proxy @ (HistoryAPI))

  
app :: JSM ()
app = do
  addInlineStyle $ decodeUtf8 $(embedFile "./assets/tailwind.min.css")
  fullPageSPA @(SPA JSM) runAppC runParDiff (withHydration ainit) view getBody onRouteChange routes


main :: IO ()
main = runJSorWarp 8080 app
