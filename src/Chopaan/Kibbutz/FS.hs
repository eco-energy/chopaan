{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds#-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Chopaan.Kibbutz.FS where

import Algebra.Graph.Label (Distance, distance, finite, getDistance, getFinite)
import qualified Chopaan.Graph.Algebraic as AG
import Chopaan.Kibbutz.KbtzId (KbtzId (..), KbtzName)
import Chopaan.Node.Components ()
import Chopaan.Node.HW (HW (..))
import Chopaan.Node.NodeId ( HHId(..), NodeMAC, NodeId(..), toText )

import Control.Applicative ()
import qualified Control.Concurrent.STM as STM
import Control.Monad ( join, (<=<) )
import Control.Monad.Catch ( MonadCatch, Exception, MonadThrow )
import Control.Monad.IO.Class (MonadIO, liftIO)


import qualified Codec.Winery as W
import ConCat.Free.VectorSpace ( distSqr, HasV(toV) )
import ConCat.Isomorphism
import ConCat.Misc ( R, Unop )


import Data.Function ( on )
import Data.Incremental
import Data.Bifunctor ( Bifunctor(bimap) )
import GHC.Generics.Lens ()
import Control.Lens ()
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid ( Sum(Sum) )
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
import qualified Streamly.Internal.FileSystem.Event.Linux as EvL
import Streamly.Internal.FileSystem.Event.Linux (Event(..), Toggle(..))


import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Prelude as S
import Streamly.Unicode.Stream (decodeUtf8, encodeUtf8)
import System.Directory
    ( removeDirectory, createDirectoryIfMissing )
import Path.IO
import Path
    ( Path,
      Dir,
      Rel,
      Abs,
      File,
      parent,
      dirname,
      toFilePath,
      (</>),
      filename,
      parseRelDir,
      parseRelFile )


newtype ArrPath = ArrPath { unArrPath :: Array.Array Word8 }
  deriving (Eq, Ord, Show, Generic)

type FPArrIso = Iso (->) FilePath ArrPath

fpArrIso :: FPArrIso
fpArrIso = Iso arrPath fromArrPath
  where
    arrPath = ArrPath . unsafePerformIO . Array.fromStreamD
              . S.toStreamD . encodeUtf8 @IO @S.SerialT . S.fromList
    fromArrPath = unsafePerformIO . (fmap (toFilePath . filename) . parseRelFile <=< S.toList) . decodeUtf8
                  . Array.toStream . unArrPath


type RelDir = Path Rel Dir
type AbsDir = Path Abs Dir
type RelFile = (Path Rel File)
type PathIso t = Iso (->) (Path Rel t) ArrPath



arrFilePath :: PathIso File
arrFilePath = Iso arrPath fromArrPath
  where
    arrPath = ArrPath . unsafePerformIO . Array.fromStreamD
              . S.toStreamD . encodeUtf8 @IO @S.SerialT . S.fromList . toFilePath
    fromArrPath = unsafePerformIO . (fmap filename . parseRelFile <=< S.toList) . decodeUtf8
                  . Array.toStream . unArrPath

arrDirPath :: PathIso Dir
arrDirPath = Iso arrPath fromArrPath
  where
    arrPath = ArrPath . unsafePerformIO . Array.fromStreamD
              . S.toStreamD . encodeUtf8 @IO @S.SerialT . S.fromList . toFilePath
    fromArrPath = unsafePerformIO . (fmap dirname . parseRelDir <=< S.toList) . decodeUtf8
                  . Array.toStream . unArrPath


class Root k where
  rootPath :: k -> Maybe RelDir

instance Root KbtzName where
  rootPath (KbtzId p) = parseRelDir . T.unpack $ p

class HasPath a where
  path :: a -> Maybe (Either RelDir RelFile)


parseOptional :: (forall m. MonadThrow m => FilePath -> m (Either RelDir RelFile)) -> T.Text -> Maybe (Either RelDir RelFile)
parseOptional f = f . T.unpack

instance HasPath (Tag KbtzName) where
  path = parseOptional (fmap Left . parseRelDir) . unKbtzId . unTag

instance HasPath (Tag HHId) where
  path = parseOptional (fmap Right . parseRelFile) . txt  . unTag
    where
      txt = T.replace "\"" "" . T.pack . show

instance (Root a, HasPath b) => HasPath (a, b) where
  path (a, b) = case rootPath a of
    Nothing -> Nothing
    Just rp -> bimap (rp </>) (rp </>) <$> path b


kbtzimConf :: EvL.Config -> EvL.Config
kbtzimConf =
  EvL.setAttrsModified Off
    . EvL.setRootPathEvents Off
    . EvL.setRootMoved On
    . EvL.setRootDeleted On
    . EvL.setWhenExists EvL.ReplaceIfExists
    . EvL.setOnlyDir On
    . EvL.setOneShot Off
    . EvL.setUnwatchMoved On
    . EvL.setFollowSymLinks Off
    . EvL.setRecursiveMode On


watchKbtzim :: forall m t. (MonadIO m) => Path t Dir -> S.SerialT m (Either KbtzEv NodeEv)
watchKbtzim dir = S.catMaybes $ S.map getEv $ wk dir
  where
    wk :: Path t Dir -> S.SerialT m Event
    wk d = S.before (liftIO $ ensureDir d) $
           S.hoist liftIO $
           EvL.watchWith kbtzimConf [unArrPath . isoFwd fpArrIso . toFilePath $ d]

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

interpretEv :: Command -> m ()
interpretEv = undefined

interpretE :: NodeModel -> NodeEv -> m ()
interpretE = undefined

interpretK :: MonadIO m => KbtzEv -> m ()
interpretK (CreateKbtz f) = case path f of
  Nothing -> return ()
  Just d -> case d of
    (Left dir) -> liftIO . ensureDir $ dir
    _ -> error "interpretK should only deal with Dir Paths"
interpretK (DeleteKbtz f) = case path f of
  Nothing -> return ()
  Just d -> case d of
    (Left dir) -> liftIO . removeDirectory . toFilePath $ dir
    _ -> error "interpretK should only deal with Dir Paths"

getKbtzEv :: Event -> Maybe KbtzEv
getKbtzEv ev
  | EvL.isDir ev && (EvL.isCreated ev || EvL.isDeleted ev) =
    ctor =<< (toKbtzName . EvL.getRelPath $ ev)
  | otherwise = Nothing
  where
    ctor
      | EvL.isCreated ev = Just . CreateKbtz . Tag
      | EvL.isDeleted ev = Just . DeleteKbtz . Tag
      | otherwise = const Nothing

data NodeEv
  = CreateNode KbtzName HHId
  | ReadNode KbtzName HHId
  | UpdateNode KbtzName HHId
  | DeleteNode KbtzName HHId
  deriving (Eq, Ord, Show, Generic)

toHHId :: RelFile -> Maybe (KbtzName, HHId)
toHHId p = (, n) <$> k
  where
    k = toKbtzName' $ parent p
    n = HHId . read . toFilePath . filename $ p

toKbtzName' :: RelDir -> Maybe KbtzName
toKbtzName' p = case T.splitOn "/" . T.pack . toFilePath $ p of
  [x] -> Just . KbtzId $ x
  _ -> Nothing

toKbtzName'' :: Maybe (Either RelDir RelFile) -> Maybe KbtzName
toKbtzName'' = ((either onL (const Nothing)) =<<)
  where
    onL p = case T.splitOn "/" . T.pack . toFilePath $ p of
      [x] -> Just . KbtzId $ x
      _ -> Nothing

toKbtzName :: Array.Array Word8 -> Maybe KbtzName
toKbtzName = toKbtzName' . isoRev arrDirPath . ArrPath

getNodeEv :: Event -> Maybe NodeEv
getNodeEv ev
  | not (EvL.isDir ev) && EvL.isCreated ev = uncurry CreateNode <$> getNode
  | not (EvL.isDir ev) && EvL.isAccessed ev = uncurry ReadNode <$> getNode
  | not (EvL.isDir ev) && EvL.isModified ev = uncurry UpdateNode <$> getNode
  | not (EvL.isDir ev) && EvL.isDeleted ev = uncurry DeleteNode <$> getNode
  | otherwise = Nothing
  where
    getNode = (toHHId . isoRev arrFilePath . ArrPath) . EvL.getRelPath $ ev

newtype Ownership a = Ownership a
  deriving (Eq, Ord, Show, Generic)
  deriving W.Serialise via W.WineryVariant (Ownership a)

newtype PPUId = PPUId Int
  deriving (Eq, Ord, Show, Generic)
  deriving W.Serialise via W.WineryRecord (PPUId)

data NodeModel = NodeModel
  { nodeIdx :: HHId,
    nodeMAC :: NodeMAC,
    nodeHW :: HW Double,
    nodeLocation :: (Double, Double),
    nodeOwner :: Ownership HHId,
    ppuNumber :: PPUId, 
    connectionTo :: HHId
  }
  deriving (Show, Generic)
  deriving (W.Serialise) via (W.WineryRecord NodeModel)

instance Eq NodeModel where
  (==) = (==) `on` nodeIdx

instance Ord NodeModel where
  compare = compare `on` nodeIdx

type KbtzModel = AG.Graph (Sum R) NodeModel


toKbtzG :: KbtzModel -> AG.Graph (Sum R) (NodeMAC, HW R)
toKbtzG = fmap toHWNode

toHWNode :: NodeModel -> (NodeMAC, HW Double)
toHWNode nm = (nodeMAC nm, nodeHW nm)

type Kbtzim = M.Map KbtzName KbtzModel

readDirFiles ::
  forall m a.
  (S.MonadAsync m, MonadCatch m, W.Serialise a) =>
  UF.Unfold m FilePath (Maybe (Either B.DecodeException a))
readDirFiles =
  UF.mapM
    (S.head . fmap (fmap B.fromWino) . B.decodeFile)
    (UF.mapMWithInput (\d f -> pure $ d <> "/" <> f)
     Dir.readFiles)

readKbtzDir ::
  forall m.
  (S.MonadAsync m, MonadCatch m) =>
  UF.Unfold m FilePath (Maybe (Either B.DecodeException NodeModel))
readKbtzDir = readDirFiles

listDirUF ::
  forall m.
  (S.MonadAsync m, MonadCatch m) =>
  UF.Unfold m FilePath FilePath
listDirUF = Dir.readFiles

listDirUF' ::
  forall m.
  (S.MonadAsync m, MonadCatch m) =>
  UF.Unfold m FilePath (Path Rel File)
listDirUF' = UF.map fromJust . UF.filter isJust $ parseRelFile <$> listDirUF

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

class Model m where
  

createKbtz :: forall m. (S.MonadAsync m, MonadCatch m) => KbtzName -> KbtzModel -> m ()
createKbtz k m = do
  interpretK (CreateKbtz (Tag k))
  sequence_
    . fmap (\n -> case (path (k, (Tag (nodeIdx n)))) of
               Nothing -> return ()
               Just p -> B.encodeArray (pathBoth p) . B.toWino $ n)
    . AG.vertexList $ m

readKbtz :: forall m. (S.MonadAsync m, MonadCatch m) => FilePath -> m KbtzModel
readKbtz = UF.fold topologicalFold (logMaybeEither readKbtzDir)

toTag :: k -> Tag k
toTag = Tag

deleteKbtz :: KbtzName -> Unop Kbtzim
deleteKbtz k = onKbtzEv (DeleteKbtz . toTag $ k)

readKbtzim :: forall m. (S.MonadAsync m, MonadCatch m) => AbsDir -> m Kbtzim
readKbtzim =
  UF.fold toMap
    (UF.mapMWithInput (\i d -> (convertFP i,) <$> readKbtz d) listDirUF) . toFilePath
  where
    convertFP :: FilePath -> KbtzName
    convertFP = KbtzId . T.pack . toFilePath . dirname . fromJust . parseRelDir

toMap :: (Monad m, Ord n) => FL.Fold m (n, a) (M.Map n a)
toMap = FL.foldl' (\m (n, a) -> M.insert n a m) mempty

kbtzimEnv :: forall m. (S.MonadAsync m, MonadCatch m, MonadFail m) => Path Abs Dir -> S.SerialT m Kbtzim
kbtzimEnv fp = S.scan (FL.foldlM' onEv (readKbtzim fp)) (watchKbtzim fp)

class KbtzState m a where
  handle :: a -> Ev -> m a

instance (MonadFS m) => KbtzState m Kbtzim where
  handle = onEv

onKbtzState :: (S.IsStream t, MonadFS m, KbtzState m a) => m a -> t m Ev -> t m a
onKbtzState = withEvs handle

withEvs :: (S.IsStream t, MonadFS m)
  => (a -> Ev -> m a) -> m a -> t m Ev -> t m a
withEvs = S.scanlM'

type Ev = Either KbtzEv NodeEv
type MonadFS m = (S.MonadAsync m, MonadCatch m)

onEv :: (S.MonadAsync m, MonadCatch m) => Kbtzim -> Ev -> m Kbtzim
onEv k (Left kv) = pure $ onKbtzEv kv k
onEv k (Right nv) = onNodeEv nv k

onNodeEv :: (S.MonadAsync m, MonadCatch m) => NodeEv -> Kbtzim -> m Kbtzim
onNodeEv (CreateNode k n) ks = upsertNode k n ks
onNodeEv (ReadNode _ _) ks = pure ks
onNodeEv (UpdateNode k n) ks = upsertNode k n ks
onNodeEv (DeleteNode k n) ks = pure $ removeNode k n ks


onKbtzEv :: KbtzEv -> Kbtzim -> Kbtzim
onKbtzEv (CreateKbtz p) ks = case toKbtzName'' . path $ p of
  Nothing -> ks
  Just k -> M.insert k AG.empty ks
onKbtzEv (DeleteKbtz p) ks = case toKbtzName'' . path $ p of
  Nothing -> ks
  Just k -> M.delete k ks

topologicalFold :: forall m. (S.MonadAsync m) => FL.Fold m NodeModel KbtzModel
topologicalFold = FL.foldl' addNode AG.empty

readNode ::
  forall m.
  (S.MonadAsync m, MonadCatch m) =>
  FilePath ->
  m (Maybe (Either B.DecodeException NodeModel))
readNode = S.head . fmap (fmap B.fromWino) . B.decodeFile

fromNodeModels :: [NodeModel] -> KbtzModel
fromNodeModels = foldl (addNode) AG.empty

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
    hasV (AG.Connect 0 r l) = Nothing
    hasV e =
      error ("hasV is called after an inducement, it should not return: " <> show e)

updateNode :: NodeModel -> Unop KbtzModel
updateNode v = AG.replaceVertex v v

readNode' :: (S.MonadAsync m, MonadCatch m)
  => KbtzName -> HHId -> m (Maybe (Either B.DecodeException NodeModel))
readNode' k n = case path (k, Tag n) of
  Nothing -> (liftIO . print $ ("Non-existent Path! " :: String)) >> return Nothing
  Just p -> liftIO . readNode $ pathBoth p

pathBoth :: Either RelDir RelFile -> FilePath
pathBoth = either toFilePath toFilePath

upsertNode :: (S.MonadAsync m, MonadCatch m)
  => KbtzName -> HHId -> Kbtzim -> m Kbtzim
upsertNode k n ks = do
  n' <- readNode' k n 
  case n' of
    Nothing -> return ks
    Just (Right n'') ->
      return $ M.update (Just . flip addNode n'') k ks
    Just (Left n'') -> do
      liftIO . print $ "Parsing Error: " <> show n''
      return ks

removeNode :: KbtzName -> HHId -> Unop Kbtzim
removeNode k n = M.update (Just . AG.removeVertex (NodeModel {nodeIdx = n})) k

newtype Command = Command (Either KbtzEv NodeEv)
  deriving (Eq, Ord, Show, Generic)


-- data CRUDError = CreateError | ReadError | UpdateError | DeleteError
--   deriving (Eq, Bounded, Enum, Show, Generic, Exception)

-- class (Monad m) => MonadCRUD m where
--   createM :: (HasPath k, W.Serialise a) => k -> a -> m (Either CRUDError ())
--   readM :: (HasPath k, W.Serialise a) => k -> m (Either CRUDError a)
--   updateM :: (HasPath k, Delta a, W.Serialise a) => k -> a -> m (Either CRUDError ())
--   deleteM :: HasPath k => k -> m (Either CRUDError ())
