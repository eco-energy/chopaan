{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedStrings, TypeApplications, TypeFamilies, DeriveGeneric, StandaloneDeriving, DeriveAnyClass, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TupleSections, DerivingStrategies, DerivingVia, BangPatterns #-}
module Chopaan.Comm.S3 where

import Lens.Micro
import GHC.Generics hiding (Prefix)
import Control.Arrow (second)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS -- (AWST'(..))
import Control.Monad.Catch
import Data.Conduit.Combinators (sinkLazy)

import qualified Data.Binary as B
import Data.Bifunctor (bimap, first)
import Data.Bitraversable (bisequence)
import Data.List (genericTake)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime(..))
import Data.Time.Clock.Compat (nominalDiffTimeToSeconds)
import Data.Maybe
import Data.Either


--import Network.AWS
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as TP
import qualified Network.AWS.S3.Types as S3
import qualified Network.AWS.S3.ListObjectsV2 as S3
import qualified Network.AWS.S3.GetObject as S3

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ProtoLens.Encoding (decodeMessage, encodeMessage)
import Data.ProtoLens.Message (Message)

import Chopaan.Kibbutz.AWS.Common
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.AWS.Things
import Chopaan.Utils.Time
import Chopaan.Utils.Retry

import Proto.NodeMessageSchema.NodeMessages (MeshFrame, EnergyState, RuntimeStats)
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N (cpuTime)

import Data.Int
import Data.Word
import qualified Streamly.Prelude as S
import Streamly.Prelude (IsStream, MonadAsync, adapt)
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Array.Stream.Foreign as A
import qualified Streamly.Internal.FileSystem.File as FL
import qualified Streamly.Internal.Data.Stream.IsStream  as S
import Streamly.Internal.Data.Time.Units
import qualified Streamly.Internal.Data.Stream.IsStream.Generate as S
import qualified Streamly.External.ByteString as SBS
import qualified Streamly.External.ByteString.Lazy as SBL
import System.Directory
--import Streamly.Binary

--import qualified Streamly.Internal.Data.Stream.IsStream  as S
import Control.Monad.Trans.Resource
import Chopaan.Comm.Address
import Chopaan.Comm.Dispatch


data HydrationError = DownloadError T.Text | ParsingError T.Text
  deriving (Show, Generic)
  deriving anyclass (B.Binary)

deriving anyclass instance B.Binary (S3.ObjectKey)

--deriving anyclass instance B.Binary (SomeException)

downloadMF :: forall m. (MonadIO m, MonadCatch m)
                => Env
                -> S3.BucketName
                -> S3.ObjectKey
                -> m (Either SomeException (Either String MeshFrame))
downloadMF env bucket n = do
  obj <- liftIO $ handleAll (pure . Left)
                            (Right <$> (withAwsEnv env (readObject bucket n)))
  let mf = (decodeMessage) <$> obj
  return $ mf


downloadMF' :: forall m. (MonadIO m, MonadCatch m)
                => Env
                -> S3.BucketName
                -> S3.ObjectKey
                -> m (Either HydrationError MeshFrame)
downloadMF' env bucket n = do
  obj <- liftIO $ handleAll (pure . Left . DownloadError . T.pack . show)
                            (Right <$> (withAwsEnv env (readObject bucket n)))
  let mf = (bimap (ParsingError . T.pack . show) id . decodeMessage) <$> obj
  return $ (join mf)
        


readObject :: forall m. (MonadIO m, MonadCatch m)
           => S3.BucketName -> S3.ObjectKey -> AWST' Env (ResourceT m) BS.ByteString
readObject bucket k = timeout 120 $ do
      x <- send $ S3.getObject bucket k
      (BL.toStrict) <$> ((x ^. S3.gorsBody) `sinkBody` sinkLazy)


firstPath bucket n = do
  o <- send req
  return $ o ^? S3.lovrsContents . _head . S3.oKey
  where
    req = S3.listObjectsV2 bucket
          & S3.lovPrefix .~ (s3Prefix n)
          & S3.lovMaxKeys .~ (Just 1)

s3Paths :: forall m. (MonadIO m, MonadCatch m)
        => UF.Unfold (AWST' Env (ResourceT m)) S3.ListObjectsV2 (S3.ObjectKey)
s3Paths = let
  plist = UF.map (((^. S3.oKey) <$>) . (^. S3.lovrsContents)) pageUF
  in UF.many plist UF.fromList

s3Paths' :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
        => Env -> S3.ListObjectsV2 -> t m S3.ObjectKey
s3Paths' env req = let
  plist = S.mapM (pure . ((^. S3.oKey) <$>) . (^. S3.lovrsContents)) (pageS env req)
  in S.concatMapWith (S.ahead) S.fromList plist

s3Prefix :: (Address n) => n -> Maybe T.Text
s3Prefix = Just . stateTopic

timedPrefix :: Address n => n -> T.Text -> Maybe T.Text
timedPrefix n t = (<> ("/" <> t)) <$> (s3Prefix n)

worldStart :: MilliSecond64
worldStart = MilliSecond64 1607478885000

data Resolution = Year | Month | Week | Day | Hour | Minute | Second

resDiff r = case r of
  Second -> 1
  Minute -> 60 * (resDiff Second)
  Hour -> 60 * (resDiff Minute)
  Day -> 24 * (resDiff Hour)
  Month -> 30 * (resDiff Day)
  Week -> 7 * (resDiff Day)
  Year -> 365 * (resDiff Day) 


sigBits = (9 -) . (round . (logBase 10)) . resDiff

newtype Prefix = Prefix { unPrefix :: T.Text }
  deriving (Eq, Ord, Show, Generic, B.Binary)

prefixRange :: (IsStream t, MonadAsync m) => Resolution -> UTCTime -> UTCTime -> t m Prefix
prefixRange r t t' = S.mapM (pure . Prefix . glompPrefix) $ S.enumerateFromTo start end
  where
    significand = sigBits r
    glompPrefix :: Int64 -> T.Text
    glompPrefix !x = T.pack . show $ x
    start :: Int64
    start = unDigits 10 $ take significand $ digits 10 $ utcToSeconds t
    end = unDigits 10 $ take significand $ digits 10 $ utcToSeconds t'
    utcToSeconds = (round @_ @Int64) . nominalDiffTimeToSeconds
                       . TP.utcTimeToPOSIXSeconds
    digits !n !n' = reverse . fromJust $ mDigitsRev n n' 
    mDigitsRev :: Integral n
      => n         -- ^ The base to use.
      -> n         -- ^ The number to convert to digit form.
      -> Maybe [n] -- ^ Nothing or Just the digits of the number in list form, in reverse.
    mDigitsRev base i = if base < 1
                    then Nothing -- We do not support zero or negative bases
                    else Just $ dr base i
      where
        dr _ 0 = []
        dr b x = case base of
                   1 -> genericTake x $ repeat 1
                   _ -> let (rest, lastDigit) = quotRem x b in lastDigit : dr b rest

unDigits :: Integral n
  => n   -- ^ The base to use.
  -> [n] -- ^ The digits of the number in list form.
  -> n   -- ^ The original number.
unDigits base = foldl (\ a b -> a * base + b) 0

--getPrefixes :: (IsStream t, MonadAsync m) => Resolution -> UTCTime -> UTCTime -> t m T.Text
--getPrefixes r t = timerange r t

-- nodeMACPath :: NodeMAC -> FilePath
-- nodeMACPath = T.unpack . unNodeId -- . (T.replace ":" "_")


-- foldNodeHydration :: forall m a e1 e2.
--   (MonadAsync m, MonadCatch m, Message a)
--   => (a -> PB a)
--   -> (S3.ObjectKey -> e1 -> (Txt S3.ObjectKey, Bin T.Text))
--   -> (S3.ObjectKey -> e2 -> (Txt S3.ObjectKey, Bin T.Text))
--   -> FilePath
--   -> (T.Text -> FilePath)
--   -> (T.Text -> FilePath)
--   -> T.Text
--   -> T.Text
--   -> FL.Fold m (S3.ObjectKey, Either e1 (Either e2 a)) ()
-- foldNodeHydration serData serErrA serErrB dataPath errPath errVal err1Tag err2Tag = fmap (const ())
--   (FL.lmap bimapEncode
--     (FL.partition
--       (FL.unzip (encodeFold (errPath err1Tag)) (encodeFold (errVal err1Tag)))
--       (FL.partition
--         (FL.unzip (encodeFold (errPath err2Tag)) (encodeFold (errVal err2Tag)))
--         (encodeFold (dataPath)))))
--   where
--     bimapEncode (p, e) = bimap (serErrA p) (bimap (serErrB p) (serData)) $ e

-- data HydrationC n = RootD FilePath
--   | PathD (HydrationC n)

-- dataDir :: FilePath
-- dataDir = "./data/hydration/"

-- fetchDir :: NodeMAC -> FilePath
-- fetchDir n = dataDir <> nodeMACPath n <> "/fetch/" 

-- prefixFile n = fetchDir n <> "prefixes"

-- prefixFetchDir :: NodeMAC -> Prefix -> FilePath
-- prefixFetchDir n (Prefix t)= fetchDir n <> (T.unpack t)

-- pathDir :: NodeMAC -> Prefix -> FilePath
-- pathDir n t = (prefixFetchDir n t) <> "/paths/"

-- pathFile :: NodeMAC -> Prefix -> T.Text -> FilePath
-- pathFile n t f = pathDir n t <> (T.unpack f) 

-- errorDir :: NodeMAC -> Prefix -> FilePath
-- errorDir n t = (prefixFetchDir n t) <> "/errors/"

-- frameDir :: NodeMAC -> Prefix -> FilePath
-- frameDir n t = (prefixFetchDir n t) <> "/frames/"

-- nodeFrameFile :: NodeMAC -> Prefix -> FilePath
-- nodeFrameFile = undefined


-- binaryArray :: (MonadAsync m, B.Binary a) => a -> m (A.Array Word8)
-- binaryArray a = A.toArray (SBL.toChunks . B.encode $ a)

-- $ Setup the hydration step by creating the directory tree that holds the persistent hydration state,
-- $ if they don't exist already. If they do, parse the appropriate startAfter object
-- $ for each prefix in the range. 
hydrationSetup :: (IsStream t, MonadAsync m, MonadCatch m) => NodeMAC -> (UTCTime, UTCTime) -> m (t m Prefix) --, Maybe S3.ObjectKey) 
hydrationSetup n (startT, endT) = do
  --liftIO $ createDirectoryIfMissing True (fetchDir n)
  let prefixes = -- S.tap (FL.lmap (toTxt . unPrefix)
        --                 (encodeFold (prefixFile n)))
        -- $ S.trace (liftIO . createAllDirs)
        -- $ 
        S.uniq $ prefixRange Hour startT endT
  return prefixes
  -- where
  --   createAllDirs t = do
  --     cd (errorDir n t)
  --     cd (frameDir n t)
  --     cd (pathDir n t)
  --       where
  --         cd = createDirectoryIfMissing True

nodeS3 :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
       => Env
       -> S3.BucketName
       -> (UTCTime, UTCTime)
       -> NodeMAC
       -> Maybe S3.ObjectKey
       -> t m (Either EnergyState RuntimeStats)
nodeS3 env bucket (startT, endT) n startAfter = S.maxBuffer 100 $ S.concatM $ do
  prefixes <- hydrationSetup n (startT, endT)
  let prefixPaths t = -- S.tap (FL.lmap asA (encodeFold (pathFile n t "paths"))) $
        --S.tapRate 10 (\x -> liftIO . print $ np <> (T.unpack $ unPrefix t) <> " pathRate: " <> (show x))
        s3Paths' env (req t)
        --  $ S.unfold s3Paths (req t)
        --where
          --asA (S3.ObjectKey k) = toTxt k
          
      prefixFrames :: Prefix -> t m (S3.ObjectKey, MeshFrame)
      prefixFrames t = --S.tapRate 10 (\r -> liftIO . print $ np <> " downloadRate " <> (T.unpack $ unPrefix t) <> (show r)) $
        S.rights $ S.rights $ S.map (\(!k, !e) -> (fmap (k,)) <$> e)
        --  $ (S.tap storeAll)
        -- S.|$ S.trace (liftIO . print)
        S.|$ S.mapM downloadWithErrLog
        S.|$ {-# SCC prefixPaths_t #-} prefixPaths t
        where
          -- storeAll = foldNodeHydration @m @MeshFrame toPB
          --   (curry ((bimap (toTxt . unObject) (toTxt . T.pack))))
          --   (curry ((bimap (toTxt . unObject) (toBin . T.pack))))
          --   dataPath errPath errVal err1Tag err2Tag
          -- dataPath = (frameDir n t) <> "meshframe"
          -- errPath tag = (errorDir n t) <> (T.unpack tag)
          -- errVal tag = (frameDir n t) <> (T.unpack tag)
          -- err1Tag = "DownloadError"
          -- err2Tag = "ParsingError"
  return $ S.tapRate 10 (liftIO . (print . (prefix <>) . show)) $
    S.mapMaybeM ({-# SCC validateMF #-} uncurry validateMF) $ S.concatMapWith (S.ahead) prefixFrames prefixes
  where
    np = (T.unpack . unNodeId $ n)
    prefix = "validMF Rate " <> (T.unpack . unNodeId $ n) <> " rate: "
    onTime (_, (_, a)) (_, (_, b)) = fromMaybe EQ $ liftA2 compare a b
    unObject (S3.ObjectKey k) = k
    req (Prefix t) = S3.listObjectsV2 bucket
          & S3.lovPrefix .~ (timedPrefix n t)
          & S3.lovStartAfter .~ (fmap unObject startAfter)
    downloadWithErrLog :: S3.ObjectKey -> m (S3.ObjectKey, Either SomeException (Either String MeshFrame)) 
    downloadWithErrLog p = (p,) <$> {-# SCC "downloadMF" #-} downloadMF env bucket p
    


validateMF :: MonadIO m
  => S3.ObjectKey
  -> MeshFrame
  -> m (Maybe (Either EnergyState RuntimeStats))
validateMF k m = case accessEnergyState m of
  Just !e -> return . Just . Left $ fixGridTS t e
  Nothing ->
    case accessRTS m of
      Just !r -> return . Just . Right $ fixMeshTS t r
      Nothing -> do
        liftIO . print $ "Parse Meshframe Failed: Not ES or RTS"
        return Nothing
  where
    t = parseTime k
    toNodeMAC :: S3.ObjectKey -> Maybe (NodeMAC, Time.UTCTime)
    toNodeMAC (S3.ObjectKey txt) = do
      (nodePath, filename) <- cleanMAC txt
      nodeId <- topicToNodeId "/state/" nodePath
      ts <- Just . parseUTCTimeMS $ filename
      return (nodeId, ts)
    parseTime :: S3.ObjectKey -> Maybe Time.UTCTime
    parseTime (S3.ObjectKey k') = (fmap (parseUTCTimeMS . snd)) . cleanMAC $ k'
    cleanMAC :: T.Text -> Maybe (T.Text, T.Text)
    cleanMAC = Just . (T.breakOnEnd ("/")) . (T.replace " " "")

      
fixGridTS :: Maybe UTCTime
          -> EnergyState
          -> EnergyState
fixGridTS Nothing r = r
fixGridTS (Just t) r = case r ^? N.cpuTime of
    Nothing -> r & N.cpuTime .~ (timeToUIntSeconds t)
    (Just t') -> case (t' == 0) of
      True -> r & N.cpuTime .~ (timeToUIntSeconds t)
      False -> r

fixMeshTS :: Maybe UTCTime
          -> RuntimeStats
          -> RuntimeStats
fixMeshTS Nothing r = r
fixMeshTS (Just t) r = case r ^? N.cpuTime of
    Nothing -> r & N.cpuTime .~ (timeToUIntSeconds t)
    (Just t') -> case (t' == 0) of
      True -> r & N.cpuTime .~ (timeToUIntSeconds t)
      False -> r


metadataKey :: S3.ObjectKey
metadataKey = "chopaanMetadata"

bucketN :: S3.BucketName
bucketN = S3.BucketName "dosti-datastream"



partitionEither :: (IsStream t, Monad m) => S.SerialT m (Either a b) -> m (t m a, t m b)
partitionEither = S.foldr (either left' right') (S.nil, S.nil)
  where
    left' a ~(l, r) = (S.cons a l, r)
    right' a ~(l, r) = (l, S.cons a r)


partitionEither' :: (IsStream t, Monad m) => t m (Either a b) -> (t m a, t m b)
partitionEither' s = (S.lefts s, S.rights s)
-- createBucketIndex :: IO ()
-- createBucketIndex = do
--   l <- liftIO $ newLogger Info stdout
--   let xs = (\x -> (toNodeMAC x, x))
--         <$> (s3Paths @S.ParallelT l bucketN (Just "/kibbutz/") Nothing)
--   S.fold 
--   return ()
-- -- What would the structure of the metadata be?
-- -- Is there a greskell expression to fetch all nodes and edges in a graph?
-- fetchMetadata = undefined

-- writeMetadata = undefined

-- sensorS3 :: (IsStream t, MonadAsync m) =>  Logger -> S3.BucketName -> NodeMAC -> t m (EnergyState)
-- sensorS3 l bucket n = S.map ((fromLeft undefined) . snd)
--                   S.|$ S.filter (isLeft . snd)
--                   S.|$ nodeS3 l bucket n Nothing

-- rsS3 :: (IsStream t, MonadAsync m) => Logger -> S3.BucketName -> NodeMAC -> t m (RuntimeStats)
-- rsS3 l bucket n = S.map ((fromRight undefined) . snd)
--          $ S.filter (isRight . snd)
--          $ nodeS3 l bucket n Nothing


-- tapCount' :: forall t m a. (IsStream t, MonadAsync m, Show a) => String -> t m a -> t m a
-- tapCount' n = S.tap (printCount' n)
-- printCount' s = FL.foldlM' (\x a ->
--                               (liftIO . print $ s <> ": " <> (show x))
--                               >> (return $ x + (1 :: Int))) (pure 1)
