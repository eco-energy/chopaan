{-# LANGUAGE KindSignatures, FlexibleContexts, ScopedTypeVariables, TypeApplications, RankNTypes #-}
module Chopaan.Kibbutz.Kibbutz where

import Streamly
import qualified Streamly.Prelude as S

import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader

import Chopaan.Comm.Comm ( Address
                         , Dispatch
                         , NodeQueue
                         , subStream
                         )
import Chopaan.Node.Node ( nodeS
                         , NodeS
                         )
import Chopaan.Node.NodeId ( NodeMAC
                           , NodeId(..)
                           )
import Chopaan.Kibbutz.AWS.Things ( getThings
                                  , thingName
                                  )

import Proto.NodeMessageSchema.NodeMessages ( RuntimeStats
                                            , EnergyState
                                            )
import Chopaan.Utils.StreamsInterop (toEvent)
import qualified Reflex as R
import Reflex.Vty (VtyWidget)
--data Kbtz

type KbtzId = Text

newtype Kbtz (t :: (* -> *) -> * -> *) (m :: * -> *) n a = Kbtz {
  unKibbutz :: Map n (t m a)
}

instance (IsStream t, Monad m) => Functor (Kbtz t m n) where
  fmap f (Kbtz m) = Kbtz $ fmap (S.map f) m

kbtz
  :: (IsStream t, MonadAsync m, Address n, Dispatch b)
  => [n]
  -> (n -> t m b)
  -> (t m b -> t m a)
  -> m (Kbtz t m n a)
kbtz nodes subscribe process = do
  return . Kbtz . M.fromList $ [(n, process s) | n <- nodes, s <- subscribe <$> nodes]

sensorKbtz :: forall t m. (IsStream t, MonadAsync m)
  => [NodeMAC]
  -> NodeQueue NodeMAC EnergyState
  -> m (Kbtz t m NodeMAC NodeS)
sensorKbtz ns q = kbtz ns (sub @t @m @NodeMAC @EnergyState q) nodeS

rsKbtz :: forall t m. (IsStream t, MonadAsync m)
  => [NodeMAC]
  -> NodeQueue NodeMAC RuntimeStats
  -> m (Kbtz t m NodeMAC RuntimeStats)
rsKbtz ns q = kbtz ns (sub @t @m @NodeMAC @RuntimeStats q) id

logsKbtz :: forall t m a. (IsStream t, MonadAsync m, Dispatch a)
  => [NodeMAC]
  -> NodeQueue NodeMAC a
  -> m (Kbtz t m NodeMAC a)
logsKbtz ns q = kbtz ns (sub @t @m @NodeMAC @a q) id

asFRPNetwork :: forall t t' m m' n a.
  (IsStream t, MonadAsync m, R.Reflex t', R.TriggerEvent t' m', MonadIO m', Show a)
  => (forall x. m x -> IO x) -> Kbtz t m n a -> VtyWidget t' m' (Map n (R.Event t' a))
asFRPNetwork h = sequence . (M.map (toEvent @t @t' h)) . unKibbutz


--asFRPIO = asFRPNetwork @_ @_ @VtyWidget _ IO

sub :: forall t m n a. (IsStream t, MonadAsync m, Address n, Dispatch a)
  => NodeQueue n a
  -> n
  -> t m a
sub q t = S.map snd $ S.filter (\x -> fst x == t) $ subStream @t @m @n @a q

getNodes :: (MonadIO m) => ReaderT KbtzId m [NodeMAC]
getNodes = do
  n <- ask
  ((fmap $ NodeId . fromJust . thingName)
              <$> (liftIO . getThings $ n))
