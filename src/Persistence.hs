{-# LANGUAGE RankNTypes #-}
module Persistence where

import qualified Data.Time as Time

newtype Timespan f a = Time { unTspan :: f (Time.UTCTime, a) }

instance (Functor f) => Eq (Timespan f a) where
  t' == t = (getSpanTimestamps t') == (getSpanTimestamps t) 

getSpanTimestamps :: forall f a. (Foldable f) => Timespan f a -> (Time.UTCTime, Time.UTCTime)
getSpanTimestamps s = (foldl max mempty s' , foldl min mempty s')
  where
    s' :: f Time.UTCTime
    s' = (fmap fst $ unTspan s)

