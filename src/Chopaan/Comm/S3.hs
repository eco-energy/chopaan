{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedStrings, TypeApplications, TypeFamilies, DeriveGeneric, StandaloneDeriving, DeriveAnyClass, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Chopaan.Comm.S3 where

import Lens.Micro

import GHC.Generics
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.AWS -- (AWST'(..))
import Control.Monad.Catch
import Control.Arrow
import Data.Conduit.Combinators (sinkLazy)

import Data.List (sort, genericTake)
import qualified Data.Text as T
import Data.Time (UTCTime(..), DiffTime)
import Data.Time.Clock.Compat (nominalDiffTimeToSeconds)
import Data.Maybe
import Data.Either
import Data.Void

--import Network.AWS
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as TP
import Network.AWS.S3 (s3)
import qualified Network.AWS.S3.Types as S3
import qualified Network.AWS.S3.ListObjectsV2 as S3
import qualified Network.AWS.S3.GetObject as S3

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.ProtoLens.Encoding (decodeMessage)

import Chopaan.Kibbutz.AWS.Common
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.AWS.Things
import Chopaan.Utils.Time
import Chopaan.Utils.Retry

import Proto.NodeMessageSchema.NodeMessages (MeshFrame, EnergyState, RuntimeStats)

import Data.Int
import qualified Streamly.Prelude as S
import Streamly.Prelude (IsStream, MonadAsync, adapt)
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream.IsStream  as S
import Streamly.Internal.Data.Time.Units
import qualified Streamly.Internal.Data.Stream.IsStream.Generate as S
--import qualified Streamly.Internal.Data.Stream.IsStream  as S
import Control.Monad.Trans.Resource

import System.IO

import Chopaan.Comm.Address
import Chopaan.Comm.Dispatch
import Chopaan.Kibbutz.Kibbutz


toNodeMAC :: S3.ObjectKey -> Maybe (NodeMAC, Time.UTCTime)
toNodeMAC (S3.ObjectKey txt) = do
  (nodePath, filename) <- cleanMAC txt
  nodeId <- topicToNodeId "/state/" nodePath
  ts <- Just . parseUTCTimeMS $ filename
  return (nodeId, ts)

cleanMAC :: T.Text -> Maybe (T.Text, T.Text)
cleanMAC = Just . (T.breakOnEnd ("/")) . (T.replace " " "")

downloadMF :: forall m. (MonadIO m, MonadCatch m)
                => Env
                -> S3.BucketName
                -> S3.ObjectKey
                -> m (Maybe (Either String MeshFrame))
downloadMF env bucket n = (fmap decodeMessage)
                          <$> (liftIO $
                               handleAll (pure . (const Nothing))
                                (Just <$> (withAwsEnv env (readObject bucket n)))
                              )


readObject :: forall m. (MonadIO m, MonadCatch m)
           => S3.BucketName -> S3.ObjectKey -> AWST' Env (ResourceT m) BS.ByteString
readObject bucket k = do
      x <- send $ S3.getObject bucket k
      BS.concat . LBS.toChunks <$> (x ^. S3.gorsBody) `sinkBody` sinkLazy


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

prefixRange :: (IsStream t, MonadAsync m) => Resolution -> UTCTime -> UTCTime -> t m T.Text
prefixRange r t t' = S.mapM (pure . (glompPrefix))
                         $ S.enumerateFromTo start end
  where
    significand = sigBits r
    glompPrefix x = T.pack . show $ x
    start :: Int64
    start = unDigits 10 $ take significand $ digits 10 $ utcToSeconds t
    end = unDigits 10 $ take significand $ digits 10 $ utcToSeconds t'
    utcToSeconds = (round @_ @Int64) . nominalDiffTimeToSeconds
                       . TP.utcTimeToPOSIXSeconds
    digits n n' = reverse . fromJust $ mDigitsRev n n' 
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

nodeS3 :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
       => S3.BucketName
       -> (UTCTime, UTCTime)
       -> NodeMAC
       -> Maybe S3.ObjectKey
       -> t m (S3.ObjectKey, ((NodeMAC, Maybe Time.UTCTime), Either EnergyState RuntimeStats))
nodeS3 bucket (startT, endT) n startAfter = S.concatM $ do
  env <- getAwsEnv s3
  let prefixes = S.uniq $ prefixRange Hour startT endT
      pathT t = adapt $ S.hoist (liftIO . withAwsEnv env) $ S.unfold s3Paths (req t)
      paths = S.concatMapWith S.ahead pathT prefixes
      ts = S.mapM (\p -> do
                      let
                        nt = toNodeMAC p
                        nodeMAC = fmap fst nt
                        nodeTime = fmap snd nt
                      return (p, (nodeMAC, nodeTime)))
           paths
      mfs = S.tapRate 10 (liftIO . (print . (prefix <>) . show)) $ S.mapM (\(p, (n', t)) -> do
                       mf <- downloadMF env bucket p
                       case mf of
                         Nothing -> do
                           liftIO . print $ "Fetch Failed: " <> (show p)
                           return (p, ((n', t), Left "Fetch Failed!"))
                         Just f ->
                           return (p, ((n', t), f))
                   ) ts
  return $ process mfs
  where
    prefix = "node: " <> (show n) <> "rate: "
    onTime (_, (_, a)) (_, (_, b)) = fromMaybe EQ $ liftA2 compare a b
    unObject (S3.ObjectKey k) = k
    req t = S3.listObjectsV2 bucket
          & S3.lovPrefix .~ (timedPrefix n t)
          & S3.lovStartAfter .~ (fmap unObject startAfter)
    process :: forall n x. (MonadAsync n) => t n (x, ((Maybe NodeMAC, Maybe Time.UTCTime), Either String MeshFrame))
           -> t n (x, ((NodeMAC, Maybe Time.UTCTime), Either EnergyState RuntimeStats))
    process = S.mapM (pure . (second . second $ throwMFError))
              . S.filter (isRight . snd . snd)
              . unpackMAC
      where
        unpackMAC :: t n (x, ((Maybe a, Maybe c), b))
                  -> t n (x, ((a, Maybe c), b))
        unpackMAC = S.mapM (pure . (second . first . first $ fromJust))
                    . S.filter (isJust . fst . fst . snd)
        throwMFError :: Either String MeshFrame -> (Either EnergyState RuntimeStats)
        throwMFError m' = case m' of
          Left e -> error $ "nodeS3 ::" <> (show n) <> "Parse Meshframe Failed: " <> e 
          Right m ->
            case accessEnergyState m of
              Just e -> Left e
              Nothing ->
                case accessRTS m of
                  Just r -> Right r
                  Nothing -> error $ ""


metadataKey :: S3.ObjectKey
metadataKey = "chopaanMetadata"

bucketN :: S3.BucketName
bucketN = S3.BucketName "dosti-datastream"


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

