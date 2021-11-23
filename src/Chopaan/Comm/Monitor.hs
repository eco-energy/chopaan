{-# LANGUAGE BangPatterns, DataKinds, TypeOperators, DeriveGeneric, DeriveAnyClass, TypeApplications, OverloadedStrings #-}
module Chopaan.Comm.Monitor where

import Control.Monad.IO.Class
import GHC.Generics
import Data.Typeable
import Data.Bifunctor

import System.Metrics.Gauge as G
import System.Metrics.Counter as C
import System.Metrics hiding (Counter, Gauge)
import Data.Vinyl
--import Composite.Ekg
import qualified Data.Text as T
import Data.Selectors

--type NumPrefixes = "numPrefixes" :-> Gauge

-- type Monitor = Monitor' Double Integer

data Monitor = Monitor
  { numPrefixes :: !Counter
  , discoveredPaths :: !Counter
  , totalDownloadableSize :: !Counter
  , downloadedSize :: !Counter
  , downloadSpeed :: !Gauge
  , downloadedFrames :: !Counter
  , framesStored :: !Counter
  , downloadErrors :: !Counter
  , parsingErrors :: !Counter
  , validated :: !Counter
  , secondsElapsed :: !Counter
  } deriving (Generic)

instance Selectors Monitor where
  selectors = selectorsRep @Monitor

nodeMon :: (MonadIO m) => Store -> T.Text -> m (Monitor)
nodeMon store n = liftIO $ do
  cProto <- C.new
  gProto <- G.new
  xs <- mapM (uncurry (onRep cProto gProto store)) fs
  return $ toMon xs 
  where
    toMon [ (Left a), (Left b), (Left c)
          , (Left d), (Right e), (Left f)
          , (Left g), (Left h), (Left i)
          , (Left j), (Left k)] = Monitor a b c d e f g h i j k
    toMon _ = error "REALLY SHOULD MATCH THE FIELDS ON THE MONITOR CONSTRUCTOR!"
    fs = fmap (first T.pack) $ selectors @(Rep Monitor)
    onRep cp gp s f t | t == (typeOf cp) = Left <$> (createCounter (metricName f) s) 
                      | t == (typeOf gp) = Right <$> (createGauge (metricName f) s)
                      | otherwise = print t
                  >> error "Only Counter and Gauge Expected!"
    metricName t = n <> "." <> t 
  
-- printMon :: String -> Monitor -> IO ()
-- printMon l m = do
--   putStrLn l
--   putStrLn $ show m


-- incPrefixCount :: Int -> Monitor -> Monitor
-- incPrefixCount !i !m = m & #numPrefixes %~ (+ (fromIntegral i))
-- {-# INLINE incPrefixCount #-}

-- incPathCount :: Int -> Monitor -> Monitor
-- incPathCount !i !m = m & #discoveredPaths %~ (+ (fromIntegral i))
-- {-# INLINE incPathCount #-}

-- incDLableSize :: Int -> Monitor -> Monitor
-- incDLableSize !i !m = m & #totalDownloadableSize %~ (+ (fromIntegral i))
-- {-# INLINE incDLableSize #-}

-- incDLSize :: Int -> Monitor -> Monitor
-- incDLSize !i !m = m & #downloadedSize %~ (+ (fromIntegral i))
-- {-# INLINE incDLSize #-}

-- incDLCount :: Int -> Monitor -> Monitor
-- incDLCount !i !m = m & #downloadedFrames %~ (+ (fromIntegral i))
-- {-# INLINE incDLCount #-}

-- incStoredCount :: Int -> Monitor -> Monitor
-- incStoredCount !i !m = m & #framesStored %~ (+ (fromIntegral i))
-- {-# INLINE incStoredCount #-}

-- incSecondsElapsed :: Double -> Monitor -> Monitor
-- incSecondsElapsed !i !m = m & #secondsElapsed %~ (+ (round i))
-- {-# INLINE incSecondsElapsed #-}

-- incDLError :: Monitor -> Monitor
-- incDLError m = m & #downloadErrors %~ (+ 1)
-- {-# INLINE incDLError #-}

-- incParsingError :: Monitor -> Monitor
-- incParsingError m = m & #parsingErrors %~ (+ 1)
-- {-# INLINE incParsingError #-}

-- incValidated :: Int -> Monitor -> Monitor
-- incValidated !i !m = m & #validated %~ (+ (fromIntegral i))
-- {-# INLINE incValidated #-}

-- incDLSpeed :: Monitor -> Monitor
-- incDLSpeed !m = m & #downloadSpeed
--                 .~ ((fromIntegral (m ^. #downloadedSize)) / (fromIntegral $ (m ^. #secondsElapsed)))
-- {-# INLINE incDLSpeed #-}
  
-- type MonWrite m = (Monitor -> Monitor) -> m ()
