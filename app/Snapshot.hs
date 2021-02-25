{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Snapshot where

import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text, pack, unpack)
import           GHC.Generics                (Generic)
import           Safe                        (readMay)
import           Shpadoinkle                 (Html, liftC, text)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html            (div_, getBody, input', onInput,
                                              onOption, option, select, value)
import           Shpadoinkle.Run             (runJSorWarp, simple)
import           Control.DeepSeq
import           Diagrams.TwoD.GraphViz


main :: IO ()
main = runJSorWarp 8080 $
  simple runParDiff (Model Graph 0 0) view getBody

data Model = Model Graph Int Int deriving (Eq, Show, Generic, NFData)

data Graph = Graph deriving (Eq, Show, Generic, NFData)

view :: Monad m => Model -> Html m Model
view model = undefined

