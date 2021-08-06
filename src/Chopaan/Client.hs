{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, TemplateHaskell, LambdaCase  #-}

module Chopaan.Client where

import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.Reader        (MonadIO, liftIO, ReaderT(..), ask, MonadReader)
import           Data.Proxy                  (Proxy (..))

import           Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import           Servant.Streamly

#ifndef ghcjs_HOST_OS
import           Shpadoinkle                 (JSM, MonadJSM, MonadUnliftIO (..),
                                              UnliftIO (..), askJSM, runJSM, liftJSM)
#else
import           Shpadoinkle                 (JSM, MonadUnliftIO (..),
                                              UnliftIO (..), askJSM, runJSM, liftJSM)
#endif

import           Data.FileEmbed              (embedFile)
import           Data.Text.Encoding          (decodeUtf8)

import           Servant.API                 ((:<|>) (..))
import           Shpadoinkle.Backend.Snabbdom (runSnabbdom) --, stage)
import           Shpadoinkle.Backend.ParDiff (runParDiff, stage)
import           Shpadoinkle.Html.Utils      (addInlineStyle, getBody)
import           Shpadoinkle.Router          (fullPageSPA, withHydration)
import           Shpadoinkle.Router.Client   (client, runXHR', runXHR)
import           Servant.Client.JS

import           Chopaan.CRUD
import           Chopaan.UiTypes              (API, SPA,
                                              routes, Route(..), Frontend(..))
import           Chopaan.API.History
import           Chopaan.View                   (ainit, ginitM, onRouteChange, view, template)

import           Shpadoinkle.Run             (runJSorWarp, Env(Dev, Prod))

newtype AppC a = AppC { runAppC :: ReaderT ClientEnv JSM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader ClientEnv)
#ifndef ghcjs_HOST_OS
  deriving (MonadJSM)
#endif

runApp :: ClientEnv -> AppC a -> JSM a
runApp c = (flip runReaderT c) . runAppC

instance MonadUnliftIO AppC where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = do
    ctx <- askJSM
    env <- ask
    return $ UnliftIO $ \(AppC m) -> runJSM (runReaderT m env) ctx

instance CRUDChopaan AppC where
  listKibbutzim = do
    env <- ask
    liftJSM $ runXHR' listKibbutzimM env
  listNodezim k = do
    env <- ask
    liftJSM $ runXHR' (listNodezimM k) env
  getGraph k g t0 t1 = adapt . S.concatM $ do
    env <- ask
    (S.hoist (AppC . liftIO) . adapt) <$> (liftJSM $ runXHR' r env)
    where
      r = historyAPI k g t0 t1
    

getClientEnv :: String -> Int -> ClientEnv
getClientEnv host port = ClientEnv $ BaseUrl (scheme host) host port "" 
  where 
    scheme "localhost" = Http
    scheme _ = Https

(listKibbutzimM :<|> listNodezimM)
  = client (Proxy @ API)

(historyAPI)
  = client (Proxy @ (HistoryAPI AheadT))


app :: String -> Int -> JSM ()
app h p = do
  let e = getClientEnv h p
  fullPageSPA @(SPA JSM) (runApp e) runParDiff (withHydration ainit) view getBody onRouteChange routes


main :: IO ()
main = runJSorWarp 8080 (app "localhost" 8080)
