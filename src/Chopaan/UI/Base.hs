{-# LANGUAGE ConstraintKinds, ExplicitForAll, LambdaCase, ScopedTypeVariables, TypeApplications #-}
module Chopaan.UI.Base where

import Reflex.Vty
import Reflex

import Control.Monad.Fix
import Control.Monad.NodeId
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Graphics.Vty as V

import qualified Data.Text.Zipper as TZ


type UIConstraints t m = (Reflex t, Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m, MonadNodeId m)

{--
scrollingList :: (Ord n, Show a, Show a) => Map n (Event t a) -> VtyWidget t m ()
scrollingList tMap = undefined



scrollable
  :: forall t m f a. (Reflex t, MonadHold t m, MonadFix m, Functor f, Show a)
  => Event t Int
  -- ^ Number of items to scroll by
  -> VtyWidget t (f a)
  -> VtyWidget t m (Behavior t (Int, Int))
  -- ^ (Current scroll position, total number of lines)
scrollable scrollBy w = do
  dw <- displayWidth
  let imgs = wrap <$> current dw <*> t
  kup <- key V.KUp
  kdown <- key V.KDown
  m <- mouseScroll
  let requestedScroll :: Event t Int
      requestedScroll = leftmost
        [ 1 <$ kdown
        , (-1) <$ kup
        , ffor m $ \case
            ScrollDirection_Up -> (-1)
            ScrollDirection_Down -> 1
        , scrollBy
        ]
      updateIdx maxN delta ix = min (max 0 (ix + delta)) maxN
  lineIndex :: Dynamic t Int <- foldDyn (\(maxN, delta) ix -> updateIdx (maxN - 1) delta ix) 0 $
    attach (length <$> imgs) requestedScroll
  tellImages $ fmap ((:[]) . V.vertCat) $ drop <$> current lineIndex <*> imgs
  return $ (,) <$> ((+) <$> current lineIndex <*> pure 1) <*> (length <$> imgs)
  where
    wrap :: _
    wrap maxWidth = concatMap (fmap (V.string V.defAttr . T.unpack) . TZ.wrapWithOffset maxWidth 0) . T.split (=='\n')
--}
