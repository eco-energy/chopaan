{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -threaded #-}

module Chopaan.UI.Monitor where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.NodeId
import Data.Functor.Misc
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import qualified Graphics.Vty as V
import Reflex
import Reflex.Network
import Reflex.Class.Switchable
import Reflex.Vty

data Monitor = Monitor_State
             | Monitor_RuntimeStats
             | Monitor_Logs
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

type MonitorC t m = (Reflex t, Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m, MonadNodeId m)

type EventMap t m n a = MonitorC t m => Map n (Event t a)


monitor :: forall t m n a b c. (MonitorC t m, Show n, Show a, Show b, Show c)
  => EventMap t m n a -> EventMap t m n b -> EventMap t m n c
  -> VtyWidget t m (Event t ())
monitor sensors runtimeStats logs = do
  inp <- input
  let buttons = col $ do
        fixed 4 $ col $ do
          fixed 1 $ text "Select an section."
          fixed 1 $ text "Esc will bring you back here."
          fixed 1 $ text "Ctrl+c to quit."
        a <- fixed 5 $ textButtonStatic def "Grid State"
        b <- fixed 5 $ textButtonStatic def "Runtime Stats"
        c <- fixed 5 $ textButtonStatic def "Debug Logs"
        return $ leftmost
          [ Left Monitor_State <$ a
          , Left Monitor_RuntimeStats <$ b
          , Left Monitor_Logs <$ c
          ]
      escapable w = do
        void w
        i <- input
        return $ fforMaybe i $ \case
          V.EvKey V.KEsc [] -> Just $ Right ()
          _ -> Nothing
  rec out <- networkHold buttons $ ffor (switch (current out)) $ \case
        Left Monitor_State -> escapable $ thingo sensors
        Left Monitor_RuntimeStats -> escapable $ thingo runtimeStats
        Left Monitor_Logs -> escapable $ thingo logs
        Right () -> buttons
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing


thingo :: (Reflex t, MonadHold t m, MonadFix m, PostBuild t m, MonadNodeId m, Show n, Show a)
  => Map n (Event t a) -> VtyWidget t m ()
thingo m = sequence_ $ Map.mapWithKey (curry scrollingBox) m

scrollingBox :: (Reflex t, MonadHold t m, MonadFix m, PostBuild t m, MonadNodeId m, Show n, Show a)
  => (n, Event t a) -> VtyWidget t m ()
scrollingBox (n, e) = col $ do
  eb <- hold "No Event Yet" $ (T.pack . show) <$> e
  fixed 2 $ text "node: " -- <> T.pack . show $ n
  _ <- fixed 5 $ boxStatic def $ scrollableText never eb
  return ()

