{-# LANGUAGE RankNTypes, ConstraintKinds, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Chopaan.Kibbutz.TKbtzim where

import GHC.Generics

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class

import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Read (readMaybe)

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Unfold as UF

import System.Envy
    ( Var(..))

import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.NodeId
import Chopaan.Node.HW

type TNodes = TVar (KbtzHW)
type TMap k v = TVar (M.Map k (TVar v))
type KbtzHW = (M.Map NodeMAC (HW Double))
type KbtzimHW = M.Map KbtzName KbtzHW
type TKbtzim = TMap KbtzName KbtzHW


data LifeTime = Finite | Infinite deriving (Eq, Ord, Show, Generic, Read)


instance Var LifeTime where
  toVar = show
  fromVar = readMaybe



lookupTSet :: KbtzName -> TKbtzim -> STM (Maybe (Set NodeMAC))
lookupTSet k tv = do
  m <- readTVar tv
  case M.lookup k m of
    Nothing -> return Nothing
    Just s' -> Just . M.keysSet <$> readTVar s'

mkTKbtz :: KbtzimHW -> STM TKbtzim
mkTKbtz kns = newTVar =<< traverse newTVar kns


unfoldNodes :: forall m. (MonadIO m) => LifeTime -> TKbtzim -> UF.Unfold m KbtzName NodeMAC
unfoldNodes lt tv = -- traceUF (liftIO . print) $ 
  UF.many (UF.mkUnfoldM step inject) UF.fromList
  where
    delS = 10
    delay = liftIO $ threadDelay $ round $ delS * 1000000
    onNullDiff s = case lt of
      Finite -> return UF.Stop
      Infinite -> delay >> return (UF.Skip s)
    step (k, oldSet) = do
      newSet <- liftIO . atomically $ lookupTSet k tv
      case newSet of
        Nothing -> return UF.Stop
        Just s -> do
          let diff = Set.difference s oldSet
          if null diff then onNullDiff (k, s) else return $ UF.Yield (Set.toList diff) (k, s)
    inject k = return (k, mempty)

getKeys :: (Ord k) => TMap k v -> STM (Set k)
getKeys = fmap (Set.fromList . M.keys) . readTVar

traceUF :: (Monad m) => (a -> m b) -> UF.Unfold m x a -> UF.Unfold m x a
traceUF f = UF.mapM (\a -> f a >> pure a)


unfoldKbtzim :: forall m. (MonadIO m) => LifeTime -> TKbtzim -> UF.Unfold m () (KbtzName, TNodes)
unfoldKbtzim lt tv = traceUF (liftIO . print . fst) $
                  UF.many (UF.mkUnfoldM step inject) UF.fromList
  where
    delS = 10
    delay = liftIO $ threadDelay $ round $ delS * 1000000
    onNullDiff s = case lt of
      Finite -> return UF.Stop
      Infinite -> delay >> return (UF.Skip s)
    step :: Set KbtzName -> m (UF.Step (Set KbtzName) [(KbtzName, TNodes)])
    step oldSet = do
      newSet <- liftIO . atomically $ getKeys tv
      let diff = Set.difference newSet oldSet
      if null diff then onNullDiff newSet else (do
        let z = Set.toList diff
        ps <- liftIO . atomically $ traverse (`nodeSet'` tv) z
        return $ UF.Yield (zip z ps) newSet)
    inject :: () -> m (Set KbtzName)
    inject _ = return mempty

nodeSet' :: KbtzName -> TKbtzim -> STM TNodes
nodeSet' kId ks = maybe retry return . M.lookup kId =<< readTVar ks

nodeSet :: MonadIO m => KbtzName -> TKbtzim -> m TNodes
nodeSet kId = liftIO . atomically . nodeSet' kId


-- data Command = StartKbtz KbtzName [HWNode]
--              | StopKbtz KbtzName
--              | StartNode KbtzName HWNode
--              | StopNode KbtzName HWNode
--              | ShowState
--              deriving (Eq, Ord, Show, Generic)


-- parseCmd :: (HConS t m) => t m Command
-- parseCmd = S.delayPre 1 $ S.repeat ShowState

-- onCommand :: TKbtzim -> Command -> STM ()
-- onCommand tv (StartKbtz k ns) = do
--   m <- readTVar tv
--   let s = M.lookup k m
--   case s of
--     Nothing -> do
--       v <- newTVar (Set.fromList ns)
--       modifyTVar' tv (M.insert k v)
--     Just s' -> modifyTVar' s' (\s'' -> s'' <> Set.fromList ns)
-- onCommand tv (StopKbtz k) = modifyTVar' tv (M.delete k)
-- onCommand tv (StartNode k n) = do
--   m <- readTVar tv
--   let s = M.lookup k m
--   case s of
--     Nothing -> return ()
--     (Just s') -> modifyTVar s' (Set.insert n)
-- onCommand tv (StopNode k n) = do
--   m <- readTVar tv
--   let s = M.lookup k m
--   case s of
--     Nothing -> return ()
--     (Just s') -> modifyTVar s' (Set.delete n)
-- onCommand tv ShowState = void $ readKbtzim tv
-- --getKbtzim :: UF.Unfold m TKbtzim KbtzName
-- --getKbtzim = UF.unfoldrM ()
