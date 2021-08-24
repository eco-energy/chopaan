{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, TypeApplications #-}
module Chopaan.Ui.Timeline where

import           Shpadoinkle                       (Html, MonadJSM, text, voidC)
import qualified Shpadoinkle.Html                  as H
import qualified Shpadoinkle.Html.Utils            as H
import           Shpadoinkle.Lens

import Control.Lens hiding (simple)

import qualified Streamly.Data.Array.Foreign as A
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Time as Ti

import Chopaan.Utils.Time (dayRange)
import qualified Chopaan.Ui.Style as Css

import Shpadoinkle.Run (simple, runJSorWarp)
import Shpadoinkle.Backend.Snabbdom (runSnabbdom, stage)


default(T.Text)

timeline :: (Functor m)
  => (Ti.UTCTime, Ti.UTCTime)
  -> (Ti.UTCTime, Ti.UTCTime)
  -> Html m (Ti.UTCTime, Ti.UTCTime)
timeline (startG, endG) (start, end) = H.div sliderStack
  [ H.div labelledSlider
    [ H.label [] [H.text $ "Start Date :: " <> (T.pack . show $ start)]
    , generalize _1 $ rangeInput [H.onInput fromSlider] (0 :: Int) (length r)
    ]
  , H.div labelledSlider
    [ H.label [] [H.text $ "End Date :: " <> (T.pack . show $ end)]
    , generalize _2 $ rangeInput [H.onInput fromSlider] (0 :: Int) (length r)
    ]
  ]
  where
    sliderStack = [H.class' $ Css.grid <> Css.grid_flow_col <> Css.grid_cols_2]
    labelledSlider = [H.class' $ Css.grid <> Css.grid_flow_row <> Css.grid_rows_2]
    fromSlider :: T.Text -> Ti.UTCTime -> Ti.UTCTime
    fromSlider v t = fromMaybe t (getTime v)
      where
        getTime = (r V.!?) . ((flip div 10) . round . read @Float . T.unpack)
    r = V.fromList $ dayRange startG endG

rangeInput props min max = H.input (props <> [ tp "type" "range"
                                             , tp "min" (T.pack . show $ (min * 10))
                                             , tp "max" (T.pack . show $ (max * 10))
                                             , tp "step" (T.pack . show $ (10 :: Int))
                                             ]) []


tp = H.textProperty


-- main = runJSorWarp 8080 $ do
--   simple runSnabbdom (s, s) (timeline (s, e)) stage
--   where
--     s = Ti.UTCTime (Ti.fromGregorian 2021 8 4) 0
--     e = Ti.UTCTime (Ti.fromGregorian 2022 8 4) 0
