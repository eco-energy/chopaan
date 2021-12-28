{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns, TupleSections #-}
{-# LANGUAGE DataKinds #-}
module Chopaan.Kibbutz.FS where

import GHC.Generics ( Generic )
import GHC.IO.Unsafe ( unsafePerformIO )
import Control.Monad
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Catch
import qualified Control.Concurrent.STM as STM
import Control.Applicative
import ConCat.Misc
import ConCat.Free.VectorSpace

import Data.Monoid
import Data.Function
import Data.Incremental
import Data.Maybe ( isJust, fromJust, fromMaybe )
import Data.Word (Word8)
import qualified Data.Map.Strict as M

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Codec.Winery as W
import System.Directory
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.Data.Array.Foreign.Type as Array
import qualified Streamly.Internal.Data.Stream.IsStream as S

import Streamly.Internal.FileSystem.Event.Linux as Ev
    ( setAttrsModified,
      setFollowSymLinks,
      setOneShot,
      setOnlyDir,
      setRecursiveMode,
      setRootDeleted,
      setRootMoved,
      setRootPathEvents,
      setUnwatchMoved,
      setWhenExists,
      watchWith,
      Config,
      Event,
      Toggle(Off, On),
      WhenExists(ReplaceIfExists),
      showEvent
    )
import Streamly.Unicode.Stream (encodeUtf8, decodeUtf8)
import qualified Streamly.Binary as B

import Chopaan.Kibbutz.KbtzId ( KbtzName, KbtzId(..) )
import Chopaan.Node.NodeId
import qualified Streamly.Internal.FileSystem.Event.Linux as Ev
import Streamly.Internal.FileSystem.Event.Linux
    ( setAttrsModified,
      setFollowSymLinks,
      setOneShot,
      setOnlyDir,
      setRecursiveMode,
      setRootDeleted,
      setRootMoved,
      setRootPathEvents,
      setUnwatchMoved,
      setWhenExists,
      watchWith,
      Config,
      Event,
      Toggle(Off, On),
      WhenExists(ReplaceIfExists) )
import Chopaan.Node.HW (HW(..))
import Chopaan.Node.Components ()
import qualified Chopaan.Graph.Algebraic as AG
import Algebra.Graph.Label (Distance, distance, getDistance, getFinite, finite)

data FSConfig = FSConfig
  { rootDir :: ArrPath
  }
  deriving (Eq, Ord, Show, Generic)

mkFSConfig :: FilePath -> FSConfig
mkFSConfig = FSConfig . arrPath

type ArrPath = (Array.Array Word8)

arrPath :: FilePath -> ArrPath
arrPath = unsafePerformIO . (Array.fromStreamD . S.toStreamD  . encodeUtf8 @IO @S.SerialT . S.fromList)

fromArrPath :: ArrPath -> FilePath
fromArrPath = unsafePerformIO . S.toList . decodeUtf8 . Array.toStream


kbtzimConf :: Ev.Config -> Ev.Config
kbtzimConf = setAttrsModified Off
  . setRootPathEvents Off
  . setRootMoved On
  . setRootDeleted On
  . setWhenExists ReplaceIfExists
  . setOnlyDir On
  . setOneShot Off
  . setUnwatchMoved On
  . setFollowSymLinks Off
  . setRecursiveMode On

watchKbtzim' :: (MonadIO m) => FilePath -> S.SerialT m (Event) 
watchKbtzim' dir = S.before (liftIO $ createDirectoryIfMissing True dir)
  $ S.hoist liftIO
  $ watchWith kbtzimConf [arrPath dir]  

watchKbtzim :: (MonadIO m) => FilePath -> S.SerialT m (Either KbtzEv NodeEv) 
watchKbtzim dir = S.catMaybes $ S.map (getEv) $ watchKbtzim' dir

getEv :: Event -> Maybe (Either KbtzEv NodeEv)
getEv ev = case (getKbtzEv ev) of
  Just kv -> return $ Left kv
  Nothing ->  case (getNodeEv ev) of
    Just nv -> return $ Right nv
    Nothing -> Nothing


data KbtzEv = CreateKbtz ArrPath
            | DeleteKbtz ArrPath
  deriving (Eq, Ord, Show, Generic)

runKbtzEv :: MonadIO m => KbtzEv -> m ()
runKbtzEv (CreateKbtz f) = liftIO $ createDirectoryIfMissing True (fromArrPath f) 
runKbtzEv (DeleteKbtz f) = liftIO $ removeDirectory (fromArrPath f)

getKbtzEv :: Event -> Maybe KbtzEv
getKbtzEv ev
  | Ev.isDir ev && Ev.isCreated ev = Just (CreateKbtz (Ev.getRelPath ev))
  | Ev.isDir ev && Ev.isDeleted ev = Just (DeleteKbtz (Ev.getRelPath ev))
  | otherwise = Nothing


data NodeEv = CreateNode ArrPath KbtzName NodeIdx 
            | ReadNode ArrPath KbtzName NodeIdx
            | UpdateNode ArrPath KbtzName NodeIdx
            | DeleteNode ArrPath KbtzName NodeIdx 
  deriving (Eq, Ord, Show, Generic)

toNodeIdx :: ArrPath -> Maybe (KbtzName, NodeIdx)
toNodeIdx = undefined -- pure . fromArrPath

toKbtzName :: ArrPath -> Maybe KbtzName
toKbtzName = undefined

getNodeEv :: Event -> Maybe NodeEv
getNodeEv ev
  | (not (Ev.isDir ev)) && (Ev.isCreated ev) = (uncurry (CreateNode p)) <$> getNode
  | (not (Ev.isDir ev)) && (Ev.isAccessed ev) = (uncurry (ReadNode p)) <$> getNode
  | (not (Ev.isDir ev)) && (Ev.isModified ev) = (uncurry (UpdateNode p)) <$> getNode
  | (not (Ev.isDir ev)) && (Ev.isDeleted ev) = (uncurry (DeleteNode p)) <$> getNode
  | otherwise = Nothing
  where
    p = Ev.getAbsPath $ ev
    getNode = toNodeIdx . Ev.getRelPath $ ev

data NodeModel = NodeModel
  { nodeIdx :: NodeIdx
  , nodeMAC :: NodeMAC
  , nodeHW :: (HW Double)
  , nodeLocation :: (Double, Double)
  , nodeOwner :: T.Text
  , connectionTo :: NodeIdx
  }
  deriving (Show, Generic)
  deriving W.Serialise via (W.WineryRecord (NodeModel))

instance Eq NodeModel where
  (==) = (==) `on` nodeIdx

instance Ord NodeModel where
  compare = compare `on` nodeIdx

type KbtzModel = AG.Graph (Sum R) NodeModel


updateNode :: NodeModel -> Unop KbtzModel
updateNode v = AG.replaceVertex v v

addNode :: KbtzModel -> NodeModel -> KbtzModel
addNode k n = case findConn k of
      Nothing -> AG.overlay k (AG.vertex n)
      Just x -> AG.overlay k (AG.connect (Sum $ distanceTo x n) (AG.vertex x) (AG.vertex n)) 
      where
        findConn = hasV
          . AG.induce (\n' -> (connectionTo n) == (nodeIdx n'))  
        distanceTo :: NodeModel -> NodeModel -> Double
        distanceTo = (dist `on` nodeLocation)
          where
            dist :: (R, R) -> (R, R) -> R
            dist v v' = distSqr (toV v) (toV v')
        hasV (AG.Vertex n') = Just n'
        hasV AG.Empty = Nothing
        hasV e =
          error $ ("hasV is called after an inducement, it should not return: " <> (show e))

type Kbtzim = M.Map KbtzName KbtzModel

readDirFiles :: forall m a. (S.MonadAsync m, MonadCatch m, W.Serialise a)
  => UF.Unfold m FilePath (Maybe (Either B.DecodeException a))
readDirFiles = UF.mapM
  (S.head . fmap (fmap B.fromWino) . B.decodeFile)
  Dir.readFiles

readKbtzDir :: forall m. (S.MonadAsync m, MonadCatch m)
  => UF.Unfold m FilePath (Maybe (Either B.DecodeException NodeModel))
readKbtzDir = readDirFiles

readChopaanDir :: forall m. (S.MonadAsync m, MonadCatch m)
  => UF.Unfold m FilePath FilePath
readChopaanDir = Dir.readFiles

readNode :: forall m. (S.MonadAsync m, MonadCatch m)
  => FilePath -> m (Maybe (Either B.DecodeException NodeModel))
readNode = S.head . (fmap (fmap B.fromWino)) . B.decodeFile 

logMaybeEither :: (Eq a, Show x, Show e, S.MonadAsync m)
  => UF.Unfold m x (Maybe (Either e a)) -> UF.Unfold m x a
logMaybeEither = UF.map fromJust . UF.filter isJust . UF.mapMWithInput report
  where
    report _ (Just (Right x)) = return (Just x)
    report fp (Just (Left x)) = do
      liftIO . print $ "decode error for Path: " <> (show fp) <> "\n" <> (show x)
      return Nothing
    report fp (Nothing) = do
      liftIO . print $ "Nothing decoded for Path: " <> (show fp)
      return Nothing


createKbtz :: forall m. (S.MonadAsync m, MonadCatch m) => FilePath -> KbtzModel -> m ()
createKbtz fp = B.encodeArray fp . B.toWino

readKbtz :: forall m. (S.MonadAsync m, MonadCatch m) => FilePath -> m KbtzModel
readKbtz = UF.fold topologicalFold (logMaybeEither readKbtzDir)

deleteKbtz :: forall m. (S.MonadAsync m, MonadCatch m) => KbtzName -> m ()
deleteKbtz = undefined

readKbtzim :: forall m. (S.MonadAsync m, MonadCatch m) => FilePath -> m (Kbtzim)
readKbtzim = UF.fold toMap $
             (UF.mapMWithInput (\i d -> (convertFP i, ) <$> readKbtz d) readChopaanDir)
  where
    convertFP :: FilePath -> KbtzName
    convertFP = KbtzId . T.pack
    
toMap :: (Monad m, Ord n) => FL.Fold m (n, a) (M.Map n a) 
toMap = FL.foldl' (\m (n, a) -> M.insert n a m) mempty 


kbtzim :: forall m. (S.MonadAsync m, MonadCatch m)
        => FilePath -> S.SerialT m Kbtzim
kbtzim fp = S.scan (FL.foldlM' onEv (readKbtzim fp)) (watchKbtzim fp)

onEv :: (S.MonadAsync m, MonadCatch m) => Kbtzim -> Either KbtzEv NodeEv -> m Kbtzim
onEv k (Left kv) = onKbtzEv kv k 
onEv k (Right nv) = onNodeEv nv k


onNodeEv :: (S.MonadAsync m, MonadCatch m) => NodeEv -> Kbtzim -> m Kbtzim
onNodeEv (CreateNode p k _) ks = upsertFS p k ks
onNodeEv (ReadNode p k n) ks = pure ks
onNodeEv (UpdateNode p k n) ks = upsertFS p k ks
onNodeEv (DeleteNode p k n) ks = pure $
  M.update (\g -> Just $ AG.removeVertex (NodeModel {nodeIdx = n}) g) k ks

onKbtzEv :: (Applicative m) => KbtzEv -> Kbtzim -> m Kbtzim
onKbtzEv (CreateKbtz p) ks = case (toKbtzName p) of
  Nothing -> pure ks
  Just k -> pure $ M.insert k AG.empty ks
onKbtzEv (DeleteKbtz p) ks = case (toKbtzName p) of
  Nothing -> pure ks
  Just k -> pure $ M.delete k ks

topologicalFold :: forall m. (S.MonadAsync m) => FL.Fold m NodeModel KbtzModel
topologicalFold = FL.foldl' addNode AG.empty

upsertFS :: (S.MonadAsync m, MonadCatch m) => ArrPath -> KbtzName -> Kbtzim -> m Kbtzim
upsertFS p k ks = do
  n' <- readNode (fromArrPath p)
  case n' of
    Nothing -> return $ ks
    Just (Right n'') -> do
      return $ M.update (Just . updateNode n'') k ks
    Just (Left n'') -> do
      liftIO . print $ "Parsing Error: " <> (show n'')
      return $ ks
