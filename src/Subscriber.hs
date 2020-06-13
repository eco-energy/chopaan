{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Subscriber where

import Streamly
import qualified Streamly.Prelude as S

import qualified Control.Concurrent.STM.TChan as TChan
import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Maybe (isJust, fromMaybe, isNothing)

newtype Subscriber n a = Subscriber { runSubscriber :: TChan.TChan (n, a) }

newtype StreamMap n a = StreamMap { runStreamMap :: (Ord n) => HM.HashMap n (TChan.TChan (n, a)) }

--instance Functor (StreamMap t m n) where
--  fmap f (StreamMap s) = StreamMap $ fmap (S.map f) s  


mkSub :: (MonadIO m) => m (Subscriber n a)
mkSub = Subscriber <$> (liftIO . atomically $ TChan.newBroadcastTChan)

subStream :: (IsStream t, MonadAsync m, Show n, Show a) => TChan.TChan (n, a) -> ((n, a) -> Bool) -> (t m a)
subStream subChan f = S.map snd $
                      S.filter f $
                      S.repeatM (liftIO . atomically $ TChan.readTChan subChan)

writeSub :: (IsStream t, MonadAsync m) => Subscriber n a -> t m (n, a) -> t m ()
writeSub (Subscriber sub) stream = S.mapM (\s -> liftIO . atomically $ TChan.writeTChan sub $ s) $ adapt stream

subMap :: (Hashable n, Ord n, MonadIO m) => [n] -> Subscriber n a -> m (StreamMap n a)
subMap ns sub = do
  chans <- liftIO . atomically $ mapM (\_->  TChan.dupTChan $ runSubscriber sub) ns
  return $ StreamMap $ HM.fromList [(n, s) | (n,s) <- zip ns chans]

getStream :: forall t m n a . (Hashable n, Ord n, IsStream t, MonadAsync m, Show n, Show a) => StreamMap n a -> n -> t m a
getStream (StreamMap smap) k = (fromMaybe S.nil) $ (flip subStream $ (filtFn k)) <$> c
  where
    filtFn :: n -> (n, a) -> Bool
    filtFn n (n', _) = (n == n')
    c = HM.lookup k smap

getNodeChan :: (Hashable n, Ord n) => StreamMap n a -> n -> Maybe (TChan.TChan (n, a))
getNodeChan (StreamMap m)= flip HM.lookup $ m 
