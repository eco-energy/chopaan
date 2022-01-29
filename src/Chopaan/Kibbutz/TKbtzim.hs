{-# LANGUAGE RankNTypes, ConstraintKinds, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Chopaan.Kibbutz.TKbtzim where

import GHC.Generics ( Generic )

import Control.Concurrent ( threadDelay )
import Control.Concurrent.STM
    ( STM,
      TVar,
      atomically,
      newTVar,
      readTVar,
      retry,
      modifyTVar',
      orElse
    )
import Control.Monad.IO.Class ( MonadIO(..) )

import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Chopaan.Graph.Algebraic as AG
import Text.Read (readMaybe)

import qualified Streamly.Internal.Data.Unfold as UF

import System.Envy
    ( Var(..))

import Chopaan.Kibbutz.KbtzId ( KbtzName )
import Chopaan.Kibbutz.FS
    ( MonadFS,
      FsM,
      Ev,
      KbtzState(..),
      Kbtzim,
      KbtzModel,
      NodeModel(nodeIdx, nodeMAC, nodeHW),
      NodeEv(..),
      KbtzEv(..),
      Tag(Tag),
      readNode',
      watchKbtzim,
      traceUF
    )
import Chopaan.Node.NodeId ( HHId, NodeMAC )
import Chopaan.Node.HW ( HW )

type TNodes = TVar KbtzHW
type NodeHWMap = M.Map NodeMAC (HW Double)
type MACSet = TVar NodeHWMap
type TMap k v = TVar (M.Map k (TVar v))
type KbtzHW = (M.Map HHId (NodeMAC, HW Double))
type KbtzimHW = M.Map KbtzName KbtzHW
newtype TKbtzim = TKbtzim { unTKbtzim :: TMap KbtzName KbtzHW }


data LifeTime = Finite | Infinite deriving (Eq, Ord, Show, Generic, Read)

instance Var LifeTime where
  toVar = show
  fromVar = readMaybe

numKbtzimNodes :: TKbtzim -> STM Int
numKbtzimNodes t = do
  ks <- getKeys . unTKbtzim $ t
  ls <- traverse (flip numKbtzNodes t) $ Set.toList ks
  return $ foldl (+) 0 ls

numKbtzNodes :: KbtzName -> TKbtzim -> STM Int
numKbtzNodes = fmap (fmap length) . lookupTSet

lookupTSet :: KbtzName -> TKbtzim -> STM (Maybe (Set NodeMAC))
lookupTSet k (TKbtzim tv) = do
  m <- readTVar tv
  case M.lookup k m of
    Nothing -> return Nothing
    Just s' -> Just . Set.fromList . fmap (fst.snd) . M.toList <$> readTVar s'

getKbtzimHW :: TKbtzim -> STM KbtzimHW
getKbtzimHW tv = do
  m <- readTVar (unTKbtzim tv)
  traverse readTVar m

toMACSet :: KbtzHW -> NodeHWMap
toMACSet = M.fromList . fmap snd . M.toList

lookupMACSet :: KbtzName -> TKbtzim -> STM (Maybe (M.Map NodeMAC (HW Double)))
lookupMACSet k (TKbtzim tv) = do
  m <- readTVar tv
  case M.lookup k m of
    Nothing -> return Nothing
    Just s' -> Just . toMACSet <$> readTVar s'

emptyTK :: STM TKbtzim
emptyTK = fmap TKbtzim $ newTVar mempty

mkTKbtz :: KbtzimHW -> STM TKbtzim
mkTKbtz kns = fmap TKbtzim . newTVar =<< traverse newTVar kns

toKV n = (nodeIdx n, (nodeMAC n, nodeHW n))

toHWDict :: KbtzModel -> KbtzHW
toHWDict = M.fromList . fmap toKV . AG.vertexList

toKbtzimHW :: Kbtzim -> KbtzimHW
toKbtzimHW = fmap toHWDict

mkConfig :: Kbtzim -> STM TKbtzim
mkConfig =  mkTKbtz . fmap toHWDict


unfoldNodes :: forall m. (MonadIO m) => LifeTime -> TKbtzim -> UF.Unfold m KbtzName NodeMAC
unfoldNodes lt tv = UF.many (UF.mkUnfoldM step inject) UF.fromList
  where
    delS = 10 :: Double
    delay = liftIO $ threadDelay $ round $ delS * 1000000
    onNullDiff s = case lt of
      Finite -> do
        liftIO . print $ "Difference is null, stopping Finite node unfold"
        return UF.Stop
      Infinite -> do
        delay >> return (UF.Skip s)
    step (k, oldSet) = do
      newSet <- liftIO . atomically $ lookupTSet k tv
      case newSet of
        Nothing -> do
          liftIO . print $ "Stopping Node Unfold"
          return UF.Stop
        Just s -> do
          let diff = Set.difference s oldSet
          if null diff
            then onNullDiff (k, s)
            else do
            return $ UF.Yield (Set.toList diff) (k, s)
    inject k = return (k, mempty)

getKeys :: (Ord k) => TMap k v -> STM (Set k)
getKeys = fmap (Set.fromList . M.keys) . readTVar


unfoldKbtzim :: forall m. (MonadIO m) => LifeTime -> TKbtzim -> UF.Unfold m () (KbtzName, TNodes)
unfoldKbtzim lt tv = traceUF (liftIO . print . fst) $
                  UF.many (UF.mkUnfoldM step inject) UF.fromList
  where
    delS = 10 :: Double
    delay = liftIO $ threadDelay $ round $ delS * 1000000
    onNullDiff s = case lt of
      Finite -> return UF.Stop
      Infinite -> delay >> return (UF.Skip s)
    step :: Set KbtzName -> m (UF.Step (Set KbtzName) [(KbtzName, TNodes)])
    step oldSet = do
      newSet <- liftIO . atomically $ getKeys (unTKbtzim tv)
      let diff = Set.difference newSet oldSet
      if null diff then onNullDiff newSet else (do
        let z = Set.toList diff
        ps <- liftIO . atomically $ traverse (`nodeSet'` tv) z
        liftIO . print $ "New Kbtzim: " <> (show z)
        return $ UF.Yield (zip z ps) newSet)
    inject :: () -> m (Set KbtzName)
    inject _ = return mempty


nodeSet' :: KbtzName -> TKbtzim -> STM TNodes
nodeSet' kId ks = maybe retry return . M.lookup kId =<< readTVar (unTKbtzim ks)

nodeSet :: MonadIO m => KbtzName -> TKbtzim -> m TNodes
nodeSet kId = liftIO . atomically . nodeSet' kId

onEvT :: MonadFS m => TKbtzim -> Ev -> FsM m ()
onEvT k = either (onKbtzEvT k) (onNodeEvT k)

onKbtzEvT :: (MonadFS m) => TKbtzim -> KbtzEv -> FsM m ()
onKbtzEvT tv (CreateKbtz (Tag k)) = liftIO . atomically $ do
  m <- readTVar $ unTKbtzim tv
  case M.lookup k m of
    Nothing -> do
      v <- newTVar M.empty
      modifyTVar' (unTKbtzim tv) (M.insert k v)
    Just _ -> return ()
onKbtzEvT tv (DeleteKbtz (Tag k)) = liftIO . atomically $
                                    modifyTVar' (unTKbtzim tv) (M.delete k)

onNodeEvT :: (MonadFS m) => TKbtzim -> NodeEv -> FsM m ()
onNodeEvT (TKbtzim tv) (CreateNode k n) = do
  x <- readNode' k n
  case x of
    Nothing -> liftIO . print $ "Node Not Read: " <> (show n)
    Just (Left e) -> liftIO . print $ "Node Decode Error: " <> (show e)
    Just (Right nm) -> liftIO . atomically $ do
      m <- readTVar tv
      s' <- case M.lookup k m of
        Nothing -> newTVar mempty
        Just s' -> return s'
      modifyTVar' s' (uncurry M.insert (toKV nm))
      modifyTVar' tv (M.insert k s')
onNodeEvT tv (DeleteNode k n) = liftIO . atomically $ do
  m <- readTVar $ unTKbtzim  tv
  case (M.lookup k m) of
    Nothing -> return ()
    Just s' -> modifyTVar' s' (M.delete n)
onNodeEvT tv (UpdateNode k n) = onNodeEvT tv (CreateNode k n)
onNodeEvT _ (ReadNode _ _) = return ()

