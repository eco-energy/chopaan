{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedStrings, TypeApplications, TypeFamilies, DeriveGeneric, StandaloneDeriving, DeriveAnyClass, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TupleSections, DerivingStrategies, DerivingVia, BangPatterns, OverloadedLabels, RecordWildCards, DeriveFunctor, QuantifiedConstraints, InstanceSigs #-}
module Chopaan.Comm.S3 where

import Control.Lens
import GHC.Generics hiding (Prefix)
import Control.Arrow (second)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS -- (AWST'(..))
import Control.Monad.Catch
import Control.Concurrent.STM


import Data.IORef
import Data.Generics.Product
import Data.Generics.Labels
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

import Data.Conduit.Combinators (sinkLazy)

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
import Chopaan.Types (Resolution(..), BufferingOpts(..))

import Proto.NodeMessageSchema.NodeMessages (MeshFrame, EnergyState, RuntimeStats)
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N (cpuTime)

import Data.Int
import Data.Word
import qualified Streamly.Prelude as S
import Streamly.Prelude (IsStream, MonadAsync, adapt)
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold.Type as UF
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
import Streamly.Binary

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
                -> m (Either SomeException (Either String (MeshFrame), Int))
downloadMF !env !bucket !n = do
  obj <- liftIO $ handleAll (pure . Left)
                            (Right <$> (recoverC ("downloadRetry: " <> (show n)) 100 $ withAwsEnv env (readObject bucket n)))
  let mf = (first decodeMessage) <$> obj
  return $! mf
{-# INLINE downloadMF #-}

downloadMF' :: forall m. (MonadIO m, MonadCatch m)
                => Env
                -> S3.BucketName
                -> S3.ObjectKey
                -> m (Either HydrationError (MeshFrame, Int))
downloadMF' !env !bucket !n = do
  obj <- liftIO $ handleAll (pure . Left . DownloadError . T.pack . show)
                            (Right <$> (withAwsEnv env (readObject bucket n)))
  let mf = (bimap (ParsingError . T.pack . show) id . (\(o, s) -> fmap (, s) $ decodeMessage o))
        <$> obj
  return $! (join mf)
        


readObject :: forall m. (MonadIO m, MonadCatch m)
           => S3.BucketName
           -> S3.ObjectKey
           -> AWST' Env (ResourceT m) (BS.ByteString, Int)
readObject bucket k = timeout 120 $ do
  !x <- send $ S3.getObject bucket k
  let byteLen = fromIntegral $ fromMaybe 0 $ x ^. S3.gorsContentLength
  body <- (BL.toStrict . BL.take byteLen) <$> ((x ^. S3.gorsBody) `sinkBody` sinkLazy)
  return $ (body, fromIntegral byteLen) 
{-# INLINE readObject #-}

-- readObjectUF :: forall m. (MonadIO m, MonadCatch m)
--            => Env
--            -> S3.BucketName
--            -> UF.Unfold m S3.ObjectKey (Either SomeException (A.Array Word8))
-- readObjectUF env bucket = UF.functionM $ \k -> do
--   liftIO
--     $ handleAll (pure . Left)
--     $ (toArr =<< (withAwsEnv env
--                   $ recoverC ("paging retry" :: String) 1
--                   $ timeout 60
--                   $ send $ S3.getObject bucket k))
--     where
--       byteLen x = fromIntegral $ fromMaybe 0 $ x ^. S3.gorsContentLength
--       toArr x = (pure . Right) -- <$> (prefixWithLength (byteLen x)
--                 =<< (A.toArray . SBL.toChunks)
--                 =<< ((x ^. S3.gorsBody) `sinkBody` sinkLazy) --)
          
--       -- $ DOWNCASTING FROM INTEGER TO INT HERE IS ONLY OKAY BECAUSE WE ARE BUFFERING
--       -- $ PROTOBUFS WHICH ARE RATHER SMALL!
--       -- $ DO NOT USE FOR FILES WITH BYTELENGTHS LONGER THAN INT64!
      

-- firstPath bucket n = do
--   o <- send req
--   return $ o ^? S3.lovrsContents . _head . S3.oKey
--   where
--     req = S3.listObjectsV2 bucket
--           & S3.lovPrefix .~ (s3Prefix n)
--           & S3.lovMaxKeys .~ (Just 1)


-- prefixObjects :: forall m. (MonadIO m, MonadCatch m)
--   => Env
--   -> S3.BucketName
--   -> (Prefix -> S3.ListObjectsV2)
--   -> UF.Unfold m (Prefix) (S3.ObjectKey, Either SomeException (A.Array Word8)) 
-- prefixObjects env bucket f = UF.many (s3Paths'' env f) (UF.zipWith (,) (UF.function id) (readObjectUF env bucket))


-- nodeUF :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
--        => Env
--        -> S3.BucketName
--        -> (UTCTime, UTCTime)
--        -> NodeMAC
--        -> Maybe S3.ObjectKey
--        -> t m (Either EnergyState RuntimeStats)
-- nodeUF env bucket (startT, endT) n startAfter = S.mapMaybe process $
--   S.unfoldMany (prefixObjects env bucket req) $ S.adapt prefixes
--   where
--     prefixes = prefixRange Day startT endT
--     req :: Prefix -> S3.ListObjectsV2
--     req (Prefix t) = S3.listObjectsV2 bucket
--           & S3.lovPrefix .~ (timedPrefix n t)
--           & S3.lovStartAfter .~ (fmap unObject startAfter)
--     unObject (S3.ObjectKey k) = k
--     process :: (S3.ObjectKey, Either SomeException (A.Array Word8))
--             -> Maybe (Either EnergyState RuntimeStats)
--     process = (onNothing (uncurry validateMF))
--               . (second (vmE . fmap (decodeMessage . SBS.fromArray)))
--       where
--         onNothing :: ((n, a) -> Maybe b) -> ((n, Maybe a) -> Maybe b)
--         onNothing f = \(n', x) -> (curry f n') =<< x
--         vmE :: Either x (Either y z) -> Maybe z
--         vmE e = case e of
--                   (Left _) -> Nothing
--                   (Right r) -> case r of
--                     (Left _) -> Nothing
--                     Right r' -> Just r'

-- s3Paths :: forall m. (MonadIO m, MonadCatch m)
--         => UF.Unfold (AWST' Env (ResourceT m)) S3.ListObjectsV2 (S3.ObjectKey)
-- s3Paths = let
--   plist = UF.map (((^. S3.oKey) <$>) . (^. S3.lovrsContents)) pageUF
--   in UF.many plist UF.fromList


s3Paths'' :: forall m. (MonadIO m, MonadCatch m)
        => Env -> (Prefix -> S3.ListObjectsV2) -> UF.Unfold m Prefix (S3.ObjectKey, Int)
s3Paths'' env f = let
  plist = UF.map ((fmap (\a -> (a ^. S3.oKey, a ^. S3.oSize))) . (^. S3.lovrsContents))
    (UF.lmap f $ (pageUFM env))
  in UF.many plist UF.fromList
{-# INLINE s3Paths'' #-}
--s3File :: forall m. (MonadIO m, MonadCatch m)
--  => UF.Unfold m S3.ObjectKey 

s3Paths' :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
        => Env -> S3.ListObjectsV2 -> t m S3.ObjectKey
s3Paths' env req = let
  plist = S.mapM (pure . ((^. S3.oKey) <$>) . (^. S3.lovrsContents)) (pageS env req)
  in S.concatMapWith (S.ahead) S.fromList plist
{-# INLINE s3Paths' #-}

s3Prefix :: (Address n) => n -> Maybe T.Text
s3Prefix = Just . stateTopic

timedPrefix :: Address n => n -> T.Text -> Maybe T.Text
timedPrefix n t = (<> ("/" <> t)) <$> (s3Prefix n)

worldStart :: MilliSecond64
worldStart = MilliSecond64 1607478885000

resDiff :: Resolution -> Double
resDiff r = case r of
  Second -> 1
  Minute -> 60 * (resDiff Second)
  Hour -> 60 * (resDiff Minute)
  Day -> 24 * (resDiff Hour)
  Month -> 30 * (resDiff Day)
  Week -> 7 * (resDiff Day)
  Year -> 365 * (resDiff Day) 
{-# INLINE resDiff #-}


newtype Prefix = Prefix { unPrefix :: T.Text }
  deriving (Eq, Ord, Show, Generic, B.Binary)

prefixRange :: (MonadAsync m) => Resolution -> UTCTime -> UTCTime -> S.AheadT m Prefix
prefixRange !r !t !t' = S.mapM (pure . Prefix . glompPrefix) $ S.enumerateFromTo start end
  where
    sigBits = (9 -) . (round . (logBase 10)) . resDiff $ r
    glompPrefix :: Int64 -> T.Text
    glompPrefix !x = T.pack . show $ x
    start :: Int64
    start = unDigits 10 $ take sigBits $ digits 10 $ utcToSeconds t
    end = unDigits 10 $ take sigBits $ digits 10 $ utcToSeconds t'
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
{-# INLINE prefixRange #-}



nodeMACPath :: NodeMAC -> FilePath
nodeMACPath = T.unpack . unNodeId


foldNodeHydration :: forall m a e1 e2.
  (MonadAsync m, MonadCatch m, Message a)
  => (S3.ObjectKey -> a -> (Txt S3.ObjectKey, PB a))
  -> (S3.ObjectKey -> e1 -> (Txt S3.ObjectKey))
  -> (S3.ObjectKey -> e2 -> (Txt S3.ObjectKey))
  -> FilePath
  -> FilePath
  -> (T.Text -> FilePath)
  -> T.Text
  -> T.Text
  -> FL.Fold m (S3.ObjectKey, Either e1 (Either e2 a)) ()
foldNodeHydration serData serErrA serErrB dataPath dlDonePath errPath err1Tag err2Tag = fmap (const ())
  (FL.lmap bimapEncode
    (FL.partition
      (encodeFold (errPath err1Tag))
      (FL.partition
        (encodeFold (errPath err2Tag))
        (FL.unzip (encodeFold (dlDonePath)) (encodeFold (dataPath))))))
  where
    bimapEncode (p, e) = bimap (serErrA p) (bimap (serErrB p) (serData p)) $ e
    {-# INLINE bimapEncode #-}
{-# INLINE foldNodeHydration #-}


data HydrationC n = RootD FilePath
  | PathD (HydrationC n)

data HydrationDirs = HydrationDirs
  { prefixFile' :: !FilePath
  , pathFile' :: !FilePath
  , pathDir' :: !FilePath
  , errorDir' :: !FilePath
  , frameDir' :: !FilePath
  }

dataDir :: T.Text -> FilePath
dataDir time = "./data/hydration/" <> (T.unpack time) <> "/"

fetchDir :: T.Text -> NodeMAC -> FilePath
fetchDir time n = dataDir time <> nodeMACPath n <> "/fetch/" 

prefixFile time n = fetchDir time n <> "prefixes"

prefixFetchDir :: T.Text -> NodeMAC -> Prefix -> FilePath
prefixFetchDir time n (Prefix t)= fetchDir time n <> (T.unpack t)

pathDir :: T.Text -> NodeMAC -> Prefix -> FilePath
pathDir time n t = (prefixFetchDir time n t) <> "/paths/"

pathFile :: T.Text -> NodeMAC -> Prefix -> T.Text -> FilePath
pathFile time n t f = pathDir time n t <> (T.unpack f) 

errorDir :: T.Text -> NodeMAC -> Prefix -> FilePath
errorDir time n t = (prefixFetchDir time n t) <> "/errors/"

frameDir :: T.Text -> NodeMAC -> Prefix -> FilePath
frameDir time n t = (prefixFetchDir time n t) <> "/frames/"

-- nodeFrameFile :: NodeMAC -> Prefix -> FilePath
-- nodeFrameFile = undefined


-- binaryArray :: (MonadAsync m, B.Binary a) => a -> m (A.Array Word8)
-- binaryArray a = A.toArray (SBL.toChunks . B.encode $ a)

type Monitor = Monitor' Double Integer

data Monitor' r i = Monitor
  { numPrefixes :: i
  , discoveredPaths :: i
  , totalDownloadableSize :: i
  , downloadedSize :: i
  , downloadSpeed :: r
  , downloadedFrames :: i
  , framesStored :: i
  , downloadErrors :: i
  , parsingErrors :: i
  , validated :: i
  , secondsElapsed :: i
  } deriving (Show, Generic, Functor)

class IsoMon a b where
  fwd :: a -> b
  rev :: b -> a

instance IsoMon Double Integer where
  fwd = round
  rev = fromIntegral

instance IsoMon Double Int where
  fwd = round
  rev = fromIntegral

instance (forall a. IsoMon r a => IsoMon r a, Num r, RealFrac r) => Applicative (Monitor' r) where
  pure :: forall a. IsoMon r a => a -> Monitor' r a
  pure a = Monitor a a a a (rev a) a a a a a a
  liftA2 :: forall x y z. (IsoMon r x, IsoMon r y, IsoMon r z) => (x -> y -> z) -> (Monitor' r x) -> Monitor' r y -> Monitor' r z 
  liftA2 f x y = Monitor
    { numPrefixes = numPrefixes x `f` numPrefixes y
    , discoveredPaths = discoveredPaths x `f` discoveredPaths y
    , totalDownloadableSize = totalDownloadableSize x `f` totalDownloadableSize y
    , downloadedSize = downloadedSize x `f` downloadedSize y
    , downloadSpeed = rev $
                      (fwd . downloadSpeed $ x) `f` (fwd . downloadSpeed $ y)
    , downloadedFrames = downloadedFrames x `f` downloadedFrames y
    , framesStored = framesStored x `f` framesStored y
    , secondsElapsed = secondsElapsed x `f` secondsElapsed y
    , downloadErrors = downloadErrors x `f` downloadErrors y
    , parsingErrors = parsingErrors x `f` parsingErrors y
    , validated = validated x `f` validated y
    }
  
instance (IsoMon r a, RealFrac r, Num r, Num a) => Semigroup (Monitor' r a) where
  m <> m' = (+) <$> m <*> m'

newMon :: (MonadIO m) => m (IORef Monitor)
newMon = liftIO . newIORef . pure $ 0

printMon :: String -> Monitor -> IO ()
printMon !l !m = do
  putStrLn l
  putStrLn $ show m


incPrefixCount :: Int -> Monitor -> Monitor
incPrefixCount i m = m & #numPrefixes %~ (+ (fromIntegral i))
{-# INLINE incPrefixCount #-}

incPathCount :: Int -> Monitor -> Monitor
incPathCount i m = m & #discoveredPaths %~ (+ (fromIntegral i))
{-# INLINE incPathCount #-}

incDLableSize :: Int -> Monitor -> Monitor
incDLableSize i m = m & #totalDownloadableSize %~ (+ (fromIntegral i))
{-# INLINE incDLableSize #-}

incDLSize :: Int -> Monitor -> Monitor
incDLSize i m = m & #downloadedSize %~ (+ (fromIntegral i))
{-# INLINE incDLSize #-}

incDLCount :: Int -> Monitor -> Monitor
incDLCount i m = m & #downloadedFrames %~ (+ (fromIntegral i))
{-# INLINE incDLCount #-}

incStoredCount :: Int -> Monitor -> Monitor
incStoredCount i m = m & #framesStored %~ (+ (fromIntegral i))
{-# INLINE incStoredCount #-}

incSecondsElapsed :: Int -> Monitor -> Monitor
incSecondsElapsed i m = m & #secondsElapsed %~ (+ (fromIntegral i))
{-# INLINE incSecondsElapsed #-}

incDLError :: Monitor -> Monitor
incDLError m = m & #downloadErrors %~ (+ 1)
{-# INLINE incDLError #-}

incParsingError :: Monitor -> Monitor
incParsingError m = m & #parsingErrors %~ (+ 1)
{-# INLINE incParsingError #-}

incValidated :: Int -> Monitor -> Monitor
incValidated i m = m & #validated %~ (+ (fromIntegral i))
{-# INLINE incValidated #-}

incDLSpeed :: Monitor -> Monitor
incDLSpeed m = m & #downloadSpeed
                .~ ((fromIntegral (m ^. #downloadedSize)) / (fromIntegral $ (m ^. #secondsElapsed)))
{-# INLINE incDLSpeed #-}
-- getHydratedPaths :: (IsStream t, MonadAsync m, MonadCatch m)
--   => NodeMAC -> FilePath -> t m (S3.ObjectKey)
-- getHydratedPaths n f = do
  

nodeS3 :: forall m. (MonadAsync m, MonadCatch m)
       => Env
       -> S3.BucketName
       -> Resolution
       -> BufferingOpts
       -> T.Text
       -> IORef Monitor
       -> (UTCTime, UTCTime)
       -> NodeMAC
       -> Maybe S3.ObjectKey
       -> S.AheadT m (Either EnergyState RuntimeStats)
nodeS3 env bucket res BufferingOpts{..} hPrefix mon (startT, endT) n startAfter = let
  modMon = liftIO . atomicModifyIORef' mon . (fmap (, ()))
  prefixes = S.tapRate 10 (modMon . incPrefixCount)
        S.|$ S.trace (liftIO . createAllDirs)
        S.|$ S.uniq
        S.|$ S.maxBuffer prefixBuffer $  prefixRange res startT endT
  prefixPaths = (s3Paths'' env) (req)
  prefixFrames :: Prefix -> S.AheadT m (S3.ObjectKey, MeshFrame)
  prefixFrames t = S.maxBuffer frameBuffer
        S.|$ S.rights
        S.|$ S.rights
        S.|$ S.trace (\x -> if isLeft x
                            then ((modMon incDLError)
                                  >> (liftIO . print $ x))
                            else ( if (isLeft . (fromRight undefined) $ x)
                                   then ((modMon incParsingError)
                                         >> (liftIO . print $ x))
                                   else return ()))
        S.|$ S.map (\(!k, !e) -> (fmap (k,)) <$> e)
        S.|$ (S.tap (FL.rmapM (\_ -> modMon $ incStoredCount 360) $ storeAll t))
        S.|$ S.tapRate 10 (modMon . incDLCount)
        S.|$ S.mapM (\(n, x) -> case x of
                                Left l -> return $ (n, Left l)
                                Right (a, s) -> do
                                  modMon (incDLSize s)
                                  return $ (n, Right a)
                    )
        S.|$ S.mapM downloadWithErrLog
        S.|$ S.tapRate 10 (modMon . incPathCount)
        S.|$ S.tap (FL.lmap (toTxt . unObject) (encodeFold (pathFile hPrefix n t "paths")))
        S.|$ S.maxBuffer pathBuffer
        S.|$ S.mapM ((\(p, s) -> (modMon $ incDLSpeed . incDLableSize s) >> return p))
        S.|$ S.unfold prefixPaths t
  in S.tapRate 10 (\_ -> liftIO $ printMon np =<< (readIORef mon))
    S.|$ S.tapRate 10 (\_ -> modMon (incSecondsElapsed 10))
    S.|$ S.tapRate 10 (modMon . incValidated)
    S.|$ S.mapMaybe (uncurry validateMF)
    S.|$ S.concatMapWith (S.ahead) prefixFrames
    S.|$ prefixes
  where
    createAllDirs t = do
      cd (errorDir hPrefix n t)
      cd (frameDir hPrefix n t)
      cd (pathDir hPrefix n t)
        where
          cd = createDirectoryIfMissing True
    np = (T.unpack . unNodeId $ n)
    unObject (S3.ObjectKey k) = k
    req (Prefix t) = S3.listObjectsV2 bucket
          & S3.lovPrefix .~ (timedPrefix n t)
          & S3.lovStartAfter .~ (fmap unObject startAfter)
    {-# INLINE req #-}
    downloadWithErrLog :: S3.ObjectKey -> m (S3.ObjectKey, Either SomeException (Either String MeshFrame, Int)) 
    downloadWithErrLog !p = (p,) <$> downloadMF env bucket p
    {-# INLINE downloadWithErrLog #-}
    storeAll t = foldNodeHydration @m @MeshFrame (curry (bimap (toTxt . unObject) toPB))
            (curry ((toTxt . unObject) . fst))
            (curry ((toTxt . unObject) . fst))
            dataPath dlDonePath errPath err1Tag err2Tag
            where
              dlDonePath = pathFile hPrefix n t "success"
              dataPath = (frameDir hPrefix n t) <> "meshframe"
              errPath tag = (errorDir hPrefix n t) <> (T.unpack tag)
              err1Tag = "downloadError"
              err2Tag = "parsingError"
    {-# INLINE storeAll #-}
{-# INLINE nodeS3 #-}    


validateMF ::
  S3.ObjectKey
  -> MeshFrame
  -> Maybe (Either EnergyState RuntimeStats)
validateMF !k !m = case accessEnergyState m of
                     Just !e -> Just . Left $ fixGridTS t e
                     Nothing ->
                       case accessRTS m of
                         Just !r -> Just . Right $ fixMeshTS t r
                         Nothing -> Nothing
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
{-# INLINE validateMF #-}
      
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



-- partitionEither :: (IsStream t, Monad m) => S.SerialT m (Either a b) -> m (t m a, t m b)
-- partitionEither = S.foldr (either left' right') (S.nil, S.nil)
--   where
--     left' a ~(l, r) = (S.cons a l, r)
--     right' a ~(l, r) = (l, S.cons a r)


-- partitionEither' :: (IsStream t, Monad m) => t m (Either a b) -> (t m a, t m b)
-- partitionEither' s = (S.lefts s, S.rights s)

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
