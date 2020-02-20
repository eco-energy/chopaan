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


newtype Subscriber n a = Subscriber { runSubscriber :: TChan.TChan (n, a) }

newtype StreamMap t m n a = StreamMap { runStreamMap :: (IsStream t, Ord n, MonadAsync m) => HM.HashMap n (t m a) }

instance Functor (StreamMap t m n) where
  fmap f (StreamMap s) = StreamMap $ fmap (S.map f) s  


mkSub :: (MonadIO m) => m (Subscriber n a)
mkSub = Subscriber <$> (liftIO . atomically $ TChan.newBroadcastTChan)

subStream :: (IsStream t, MonadAsync m) => Subscriber n a -> ((n, a) -> Bool) -> m (t m a)
subStream (Subscriber s) f = do
  c <- (liftIO . atomically . TChan.dupTChan $ s)
  pure $ (serially $ S.map snd $ S.filter f (S.repeatM (liftIO . atomically $ TChan.readTChan c)))

writeSub :: (IsStream t, MonadAsync m) => Subscriber n a -> t m (n, a) -> t m ()
writeSub (Subscriber sub) stream = S.mapM (\s -> liftIO . atomically $ TChan.writeTChan sub $ s) $ adapt stream

subMap :: forall t m n a . (Hashable n, Ord n, IsStream t, MonadAsync m) => [n] -> Subscriber n a -> m (StreamMap t m n a)
subMap ns sub = do
  ss <- mapM nstream ns
  return $ StreamMap $ HM.fromList [(n, s) | (n,s) <- zip ns ss]
  where
    filtFn :: n -> (n, a) -> Bool
    filtFn n (n', _) = (n == n')
    nstream ::  n -> m (t m a) -- (IsStream t, MonadAsync m, Hashable a) => a -> m (t m b)
    nstream n = subStream sub (filtFn n)

getStream :: forall t m n a . (Hashable n, Ord n, IsStream t, MonadAsync m) => StreamMap t m n a -> n -> (t m a)
getStream (StreamMap smap) k = HM.lookupDefault (S.nil) k smap
