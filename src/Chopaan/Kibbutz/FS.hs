{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}
module Chopaan.Kibbutz.FS where

import GHC.Generics ( Generic )
import GHC.IO.Unsafe ( unsafePerformIO )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Catch

import qualified Data.Map.Strict as M
import Data.Maybe ( isJust, fromJust )
import Data.Word (Word8)
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
import Streamly.Unicode.Stream (encodeUtf8)
import qualified Streamly.Binary as B

import Chopaan.Kibbutz.KbtzId ( KbtzName(..) )
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


kbtzimConf :: Ev.Config -> Ev.Config
kbtzimConf = setAttrsModified Off
  . setRootPathEvents Off
  . setRootMoved Off
  . setRootDeleted On
  . setWhenExists ReplaceIfExists
  . setOnlyDir On
  . setOneShot Off
  . setUnwatchMoved On
  . setFollowSymLinks Off
  . setRecursiveMode Off

watchForKbtzim :: FilePath -> S.SerialT IO Event 
watchForKbtzim dir = S.concatM $ do
  createDirectoryIfMissing True dir
  return $ watchWith kbtzimConf [arrPath dir]  

getKbtzAction :: Event -> Maybe KbtzEv
getKbtzAction ev
  | Ev.isCreated ev = Just (CreateKbtz (Ev.getAbsPath ev))
  | Ev.isDeleted ev = Just (DeleteKbtz (Ev.getAbsPath ev))
  | otherwise = Nothing

data KbtzEv = CreateKbtz ArrPath | DeleteKbtz ArrPath
  deriving (Eq, Ord, Show, Generic)

data NodeEv = CreateNode ArrPath | ReadNode ArrPath | UpdateNode ArrPath | DeleteNode ArrPath 
  deriving (Eq, Ord, Show, Generic)

data NodeModel = NodeModel
  { nodeIdx :: NodeIdx
  , nodeMAC :: NodeMAC
  , nodeHW :: (HW Double)
  , nodeLocation :: (Double, Double)
  , nodeOwner :: T.Text
  , connectionTo :: NodeIdx
  }
  deriving (Eq, Show, Generic)
  deriving W.Serialise via (W.WineryRecord (NodeModel))

type KbtzModel = AG.Graph Double NodeModel

type Kbtzim = M.Map KbtzName KbtzModel 

readKbtzDir :: forall m. (S.MonadAsync m, MonadCatch m)
  => UF.Unfold m FilePath (Maybe (Either B.DecodeException NodeModel))
readKbtzDir = UF.mapM (S.head . fmap (fmap B.fromWino) . B.decodeFile) Dir.readFiles

reportAndClean :: (Eq a, Show x, Show e, S.MonadAsync m)
  => UF.Unfold m x (Maybe (Either e a)) -> UF.Unfold m x a
reportAndClean = UF.map fromJust . UF.filter isJust . UF.mapMWithInput report
  where
    report fp (Just (Right x)) = return (Just x)
    report fp (Just (Left x)) = do
      liftIO . print $ "decode error for Path: " <> (show fp) <> "\n" <> (show x)
      return Nothing
    report fp (Nothing) = do
      liftIO . print $ "Nothing decoded for Path: " <> (show fp)
      return Nothing
      

createKbtz :: forall m. (S.MonadAsync m, MonadCatch m) => FilePath -> KbtzModel -> m ()
createKbtz fp = B.encodeArray fp . B.toWino

readKbtz :: forall m. (S.MonadAsync m, MonadCatch m) => FilePath -> m (KbtzModel)
readKbtz = UF.fold topologicalFold (reportAndClean readKbtzDir)

readKbtzim = undefined

monitor :: (S.MonadAsync m, MonadCatch m) => FilePath -> m ()
monitor fp = do
  g <- readKbtzim
  undefined

onNodeEv :: FL.Fold m NodeEv KbtzModel
onNodeEv = undefined

onKbtzEv :: FL.Fold m KbtzEv Kbtzim
onKbtzEv = undefined



topologicalFold :: forall m. (S.MonadAsync m) => FL.Fold m NodeModel KbtzModel
topologicalFold = FL.foldl' toG AG.empty
  where
    toG :: KbtzModel -> NodeModel -> KbtzModel
    toG k n = case findConn k of
      Nothing -> (AG.vertex n)
      Just x -> AG.connect (distanceTo x n) (AG.vertex x) (AG.vertex n) 
      where
        findConn = hasV
          . AG.induce (\n' -> (connectionTo n) == (nodeIdx n'))  
        distanceTo = undefined
        hasV (AG.Vertex n') = Just n'
        hasV AG.Empty = Nothing
        hasV e =
          error $ ("hasV is called after an inducement, it should not return: " <> (show e))
