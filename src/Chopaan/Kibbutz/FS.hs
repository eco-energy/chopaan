{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}


{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE FlexibleInstances #-}

module Chopaan.Kibbutz.FS where

import Algebra.Graph.Label (Distance, distance, finite, getDistance, getFinite)
import qualified Chopaan.Graph.Algebraic as AG
import Chopaan.Kibbutz.KbtzId (KbtzId (..), KbtzName)
import Chopaan.Node.Components ()
import Chopaan.Node.HW (HW (..))
import Chopaan.Node.NodeId ( NodeIdx, NodeMAC, NodeId(..), toText )
import qualified Codec.Winery as W
import ConCat.Free.VectorSpace
import ConCat.Isomorphism
import ConCat.Incremental
import ConCat.Misc
import Control.Applicative
import qualified Control.Concurrent.STM as STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Function
import Data.Incremental
import Data.Bifunctor
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Char (ord)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.IO.Unsafe (unsafePerformIO)
import qualified Streamly.Binary as B
import qualified Streamly.Internal.Data.Array.Foreign.Type as Array
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.FileSystem.Dir as Dir
import Streamly.Internal.FileSystem.Event.Linux
  ( Config,
    Event,
    Toggle (Off, On),
    WhenExists (ReplaceIfExists),
    setAttrsModified,
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
  )
import Streamly.Internal.FileSystem.Event.Linux as Ev
  ( Config,
    Event,
    Toggle (Off, On),
    WhenExists (ReplaceIfExists),
    setAttrsModified,
    setFollowSymLinks,
    setOneShot,
    setOnlyDir,
    setRecursiveMode,
    setRootDeleted,
    setRootMoved,
    setRootPathEvents,
    setUnwatchMoved,
    setWhenExists,
    showEvent,
    watchWith,
  )
import qualified Streamly.Internal.FileSystem.Event.Linux as Ev
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Prelude as S
import Streamly.Unicode.Stream (decodeUtf8, encodeUtf8)
import System.Directory


newtype ArrPath = ArrPath { unArrPath :: Array.Array Word8 }
  deriving (Eq, Ord, Show, Generic)

instance Semigroup ArrPath where
  a <> b = isoFwd pathIso $ isoRev pathIso a <> "/" <> isoRev pathIso b

type PathIso = Iso (->) FilePath ArrPath

pathIso :: PathIso
pathIso = Iso arrPath fromArrPath
  where
    arrPath :: FilePath -> ArrPath
    arrPath = ArrPath . unsafePerformIO . Array.fromStreamD
              . S.toStreamD . encodeUtf8 @IO @S.SerialT . S.fromList
    fromArrPath :: ArrPath -> FilePath
    fromArrPath = unsafePerformIO . S.toList . decodeUtf8
                  . Array.toStream . unArrPath


class HasPath a where
  path :: a -> ArrPath
  --unpath :: ArrPath -> a

instance HasPath (Tag KbtzName) where
  path = isoFwd pathIso . T.unpack . unKbtzId . unTag

instance HasPath (Tag NodeIdx) where
  path = isoFwd pathIso . show . unNodeId . unTag

instance (HasPath a, HasPath b) => HasPath (a, b) where
  path (a, b) = path a <> path b
  -- unpath = bimap unpath unpath . joinSplit . splitPath
  --   where
  --     joinSplit = unsafePerformIO . S.uncons 
  --     splitPath (ArrPath a) = Array.splitOn (== (fromIntegral . ord $ '/')) a


kbtzimConf :: Ev.Config -> Ev.Config
kbtzimConf =
  setAttrsModified Off
    . setRootPathEvents Off
    . setRootMoved On
    . setRootDeleted On
    . setWhenExists ReplaceIfExists
    . setOnlyDir On
    . setOneShot Off
    . setUnwatchMoved On
    . setFollowSymLinks Off
    . setRecursiveMode On


watchKbtzim :: forall m. (MonadIO m) => FilePath -> S.SerialT m (Either KbtzEv NodeEv)
watchKbtzim dir = S.catMaybes $ S.map getEv $ wk dir
  where
    wk :: FilePath -> S.SerialT m Event
    wk dir = S.before (liftIO $ createDirectoryIfMissing True dir) $
             S.hoist liftIO $
             watchWith kbtzimConf [unArrPath . isoFwd pathIso $ dir]

getEv :: Event -> Maybe (Either KbtzEv NodeEv)
getEv ev = case getKbtzEv ev of
  Just kv -> return $ Left kv
  Nothing -> case getNodeEv ev of
    Just nv -> return $ Right nv
    Nothing -> Nothing

newtype Tag k = Tag { unTag :: k }
  deriving (Eq, Ord, Show, Generic)


data KbtzEv
  = CreateKbtz (Tag KbtzName)
  | DeleteKbtz (Tag KbtzName)
  deriving (Eq, Ord, Show, Generic)

interpretK :: MonadIO m => KbtzEv -> m ()
interpretK (CreateKbtz f) = liftIO . createDirectoryIfMissing True . isoRev pathIso . path $ f
interpretK (DeleteKbtz f) = liftIO . removeDirectory . isoRev pathIso . path $ f

getKbtzEv :: Event -> Maybe KbtzEv
getKbtzEv ev
  | Ev.isDir ev && Ev.isCreated ev = CreateKbtz <$> (toKbtzName . ArrPath) (Ev.getRelPath ev)
  | Ev.isDir ev && Ev.isDeleted ev = DeleteKbtz <$> (toKbtzName . ArrPath) (Ev.getRelPath ev)
  | otherwise = Nothing

data NodeEv
  = CreateNode KbtzName NodeIdx
  | ReadNode KbtzName NodeIdx
  | UpdateNode KbtzName NodeIdx
  | DeleteNode KbtzName NodeIdx
  deriving (Eq, Ord, Show, Generic)

toNodeIdx :: ArrPath -> Maybe (KbtzName, NodeIdx)
toNodeIdx p = case T.splitOn "/" . T.pack . isoRev pathIso $ p of
  [x, y] -> Just (KbtzId x, NodeId . read . T.unpack $ y)
  _ -> Nothing

toKbtzName :: ArrPath -> Maybe (Tag KbtzName)
toKbtzName p = case T.splitOn "/" . T.pack . isoRev pathIso $ p of
  [x] -> Just . Tag . KbtzId $ x
  _ -> Nothing

getNodeEv :: Event -> Maybe NodeEv
getNodeEv ev
  | not (Ev.isDir ev) && Ev.isCreated ev = uncurry CreateNode <$> getNode
  | not (Ev.isDir ev) && Ev.isAccessed ev = uncurry ReadNode <$> getNode
  | not (Ev.isDir ev) && Ev.isModified ev = uncurry UpdateNode <$> getNode
  | not (Ev.isDir ev) && Ev.isDeleted ev = uncurry DeleteNode <$> getNode
  | otherwise = Nothing
  where
    p = Ev.getAbsPath ev
    getNode = (toNodeIdx . ArrPath) . Ev.getRelPath $ ev

newtype Ownership a = Ownership a
  deriving (Eq, Ord, Show, Generic)
  deriving W.Serialise via W.WineryVariant (Ownership a)

data NodeModel = NodeModel
  { nodeIdx :: NodeIdx,
    nodeMAC :: NodeMAC,
    nodeHW :: HW Double,
    nodeLocation :: (Double, Double),
    nodeOwner :: Ownership NodeIdx,
    connectionTo :: NodeIdx
  }
  deriving (Show, Generic)
  deriving (W.Serialise) via (W.WineryRecord NodeModel)

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
    findConn =
      hasV
        . AG.induce (\n' -> connectionTo n == nodeIdx n')
    distanceTo :: NodeModel -> NodeModel -> Double
    distanceTo = dist `on` nodeLocation
      where
        dist :: (R, R) -> (R, R) -> R
        dist v v' = distSqr (toV v) (toV v')
    hasV (AG.Vertex n') = Just n'
    hasV AG.Empty = Nothing
    hasV e =
      error ("hasV is called after an inducement, it should not return: " <> show e)

type Kbtzim = M.Map KbtzName KbtzModel

readDirFiles ::
  forall m a.
  (S.MonadAsync m, MonadCatch m, W.Serialise a) =>
  UF.Unfold m FilePath (Maybe (Either B.DecodeException a))
readDirFiles =
  UF.mapM
    (S.head . fmap (fmap B.fromWino) . B.decodeFile)
    Dir.readFiles

readKbtzDir ::
  forall m.
  (S.MonadAsync m, MonadCatch m) =>
  UF.Unfold m FilePath (Maybe (Either B.DecodeException NodeModel))
readKbtzDir = readDirFiles

readChopaanDir ::
  forall m.
  (S.MonadAsync m, MonadCatch m) =>
  UF.Unfold m FilePath FilePath
readChopaanDir = Dir.readFiles

readNode ::
  forall m.
  (S.MonadAsync m, MonadCatch m) =>
  FilePath ->
  m (Maybe (Either B.DecodeException NodeModel))
readNode = S.head . fmap (fmap B.fromWino) . B.decodeFile

logMaybeEither ::
  (Eq a, Show x, Show e, S.MonadAsync m) =>
  UF.Unfold m x (Maybe (Either e a)) ->
  UF.Unfold m x a
logMaybeEither = UF.map fromJust . UF.filter isJust . UF.mapMWithInput report
  where
    report _ (Just (Right x)) = return (Just x)
    report fp (Just (Left x)) = do
      liftIO . print $ "decode error for Path: " <> show fp <> "\n" <> show x
      return Nothing
    report fp Nothing = do
      liftIO . print $ "Nothing decoded for Path: " <> show fp
      return Nothing

createKbtz :: forall m. (S.MonadAsync m, MonadCatch m) => FilePath -> KbtzModel -> m ()
createKbtz fp = B.encodeArray fp . B.toWino

readKbtz :: forall m. (S.MonadAsync m, MonadCatch m) => FilePath -> m KbtzModel
readKbtz = UF.fold topologicalFold (logMaybeEither readKbtzDir)

toTag = Tag

deleteKbtz :: KbtzName -> Unop Kbtzim
deleteKbtz k = onKbtzEv (DeleteKbtz . toTag $ k)

readKbtzim :: forall m. (S.MonadAsync m, MonadCatch m) => FilePath -> m Kbtzim
readKbtzim =
  UF.fold toMap
    (UF.mapMWithInput (\i d -> (convertFP i,) <$> readKbtz d) readChopaanDir)
  where
    convertFP :: FilePath -> KbtzName
    convertFP = KbtzId . T.pack

toMap :: (Monad m, Ord n) => FL.Fold m (n, a) (M.Map n a)
toMap = FL.foldl' (\m (n, a) -> M.insert n a m) mempty

kbtzim ::
  forall m.
  (S.MonadAsync m, MonadCatch m) =>
  FilePath ->
  S.SerialT m Kbtzim
kbtzim fp = S.scan (FL.foldlM' onEv (readKbtzim fp)) (watchKbtzim fp)

onEv :: (S.MonadAsync m, MonadCatch m) => Kbtzim -> Either KbtzEv NodeEv -> m Kbtzim
onEv k (Left kv) = pure $ onKbtzEv kv k
onEv k (Right nv) = onNodeEv nv k

kbtzFolder :: KbtzName -> FilePath
kbtzFolder (KbtzId k) = "/" <> T.unpack k

nodePath :: KbtzName -> NodeIdx -> FilePath
nodePath k n = kbtzFolder k <> "/" <> T.unpack (toText n)

onNodeEv :: (S.MonadAsync m, MonadCatch m) => NodeEv -> Kbtzim -> m Kbtzim
onNodeEv (CreateNode k n) ks = upsert' k n ks
onNodeEv (ReadNode k n) ks = pure ks
onNodeEv (UpdateNode k n) ks = upsert' k n ks
onNodeEv (DeleteNode k n) ks =
  pure $
    M.update (Just . AG.removeVertex (NodeModel {nodeIdx = n})) k ks

onKbtzEv :: KbtzEv -> Kbtzim -> Kbtzim
onKbtzEv (CreateKbtz p) ks = case toKbtzName . path $ p of
  Nothing -> ks
  Just (Tag k) -> M.insert k AG.empty ks
onKbtzEv (DeleteKbtz p) ks = case toKbtzName . path $ p of
  Nothing -> ks
  Just (Tag k) -> M.delete k ks

topologicalFold :: forall m. (S.MonadAsync m) => FL.Fold m NodeModel KbtzModel
topologicalFold = FL.foldl' addNode AG.empty

upsert' :: (S.MonadAsync m, MonadCatch m)
  => KbtzName -> NodeIdx -> Kbtzim -> m Kbtzim
upsert' k n ks = do
  n' <- liftIO . readNode . isoRev pathIso . path $ (Tag k, Tag n)
  case n' of
    Nothing -> return ks
    Just (Right n'') ->
      return $ M.update (Just . updateNode n'') k ks
    Just (Left n'') -> do
      liftIO . print $ "Parsing Error: " <> show n''
      return ks


data CRUDError = CreateError | ReadError | UpdateError | DeleteError
  deriving (Eq, Bounded, Enum, Show, Generic, Exception)

class (Monad m) => MonadCRUD m where
  createM :: (HasPath k, W.Serialise a) => k -> a -> m (Either CRUDError ())
  readM :: (HasPath k, W.Serialise a) => k -> m (Either CRUDError a)
  updateM :: (HasPath k, HasDelta a, W.Serialise a) => k -> a -> m (Either CRUDError ())
  deleteM :: HasPath k => k -> m (Either CRUDError ())
