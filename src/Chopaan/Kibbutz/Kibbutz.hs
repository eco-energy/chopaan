{-# LANGUAGE KindSignatures, FlexibleContexts, ScopedTypeVariables, TypeApplications, RankNTypes #-}
module Chopaan.Kibbutz.Kibbutz where

import Prelude hiding (zipWith)
import Streamly
import qualified Streamly.Prelude as S

import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Key

import Control.Applicative (liftA2)
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

instance (Ord n) => Semigroup (Kbtz t m n a) where
  (Kbtz a) <> (Kbtz b) = Kbtz (a <> b)

instance (Ord n) => Monoid (Kbtz t m n a) where
  mempty = Kbtz mempty

instance (IsStream t, MonadAsync m, Ord n, Monoid n) => Applicative (Kbtz t m n) where
  pure a = Kbtz $ M.singleton mempty (S.yield a)
  (Kbtz a) <*> (Kbtz b) = Kbtz $ zipWith (<*>) a b



kbtz
  :: (IsStream t, MonadAsync m, Address n, Dispatch b)
  => [n]
  -> (n -> t m b)
  -> (t m b -> t m a)
  -> Kbtz t m n a
kbtz nodes subscribe process = Kbtz . M.fromList $ [(n, process s) | n <- nodes, s <- subscribe <$> nodes]


sensorKbtz :: forall t m. (IsStream t, MonadAsync m)
  => [NodeMAC]
  -> NodeQueue NodeMAC EnergyState
  -> Kbtz t m NodeMAC NodeS
sensorKbtz ns q = kbtz ns (sub @t @m @NodeMAC @EnergyState q) nodeS

rsKbtz :: forall t m. (IsStream t, MonadAsync m)
  => [NodeMAC]
  -> NodeQueue NodeMAC RuntimeStats
  -> Kbtz t m NodeMAC RuntimeStats
rsKbtz ns q = kbtz ns (sub @t @m @NodeMAC @RuntimeStats q) id

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

asMapStream :: (IsStream t, Monad m, Monad (t m)) => Kbtz t m n a -> t m (Map n a)
asMapStream (Kbtz k) = sequence k

asStream :: forall t m n a. (IsStream t, MonadAsync m) => Kbtz t m n a -> t m (n, a)
asStream (Kbtz k) = M.foldlWithKey' (nodeTagMerge) (S.fromList []) k
  where
    nodeTagMerge :: t m (n, a) -> n -> t m a -> t m (n, a)
    nodeTagMerge c key s = (S.map (\x -> (key, x)) s) <> c
