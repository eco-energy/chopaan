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

import Chopaan.UI.Base

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
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

data Dispatches = Dispatch_Transactions
                | Dispatch_NodeConfig
                | Dispatch_MeshConfig
  deriving (Show, Read, Eq, Ord, Enum, Bounded)


type EventMap t m n a = UIConstraints t m => Map n (Event t a)


monitor :: forall t m n a b c. (UIConstraints t m, Ord n, Show n, Show a, Show b, Show c)
  => EventMap t m n a -> EventMap t m n b -> EventMap t m n c
  -> VtyWidget t m (Event t ())
monitor sensors runtimeStats logs = do
  inp <- input
  let buttons = col $ do
        fixed 4 $ col $ do
          fixed 1 $ text "Select an section."
          fixed 1 $ text "Esc will bring you back here."
          fixed 1 $ text "Ctrl+c to quit."
        stretch $ row $ do
          (monitoring) <- stretch $ col $ do
            a <- fixed 3 $ textButtonStatic def "Grid Power States"
            b <- fixed 3 $ textButtonStatic def "Runtime Stats"
            c <- fixed 3 $ textButtonStatic def "Node Logs"
            return $ Left <$> leftmost
              [ Left Monitor_State <$ a
              , Left Monitor_RuntimeStats <$ b
              , Left Monitor_Logs <$ c
              ]
          dispatching <- stretch $ col $ do
            d <- fixed 3 $ textButtonStatic def "Transactor"
            e <- fixed 3 $ textButtonStatic def "Node Config"
            f <- fixed 3 $ textButtonStatic def "Mesh Config"
            return $ Left <$> leftmost
              [ Right Dispatch_Transactions <$ d
              , Right Dispatch_NodeConfig <$ e
              , Right Dispatch_MeshConfig <$ f
              ]
          return $ leftmost [monitoring, dispatching]
      escapable w = do
        void w
        i <- input 
        return $ fforMaybe i $ \case
          V.EvKey V.KEsc [] -> Just $ Right ()
          _ -> Nothing
  rec out <- networkHold buttons $ ffor (switch (current out)) $ \case
        Left (Left Monitor_State) -> escapable $ thingo sensors
        Left (Left Monitor_RuntimeStats) -> escapable $ thingo runtimeStats
        Left (Left Monitor_Logs) -> escapable $ thingo logs
        Left (Right Dispatch_Transactions) -> escapable $ form
        Left (Right Dispatch_NodeConfig) -> escapable $ form
        Left (Right Dispatch_MeshConfig) -> escapable $ form
        Right () -> buttons
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing


thingo :: (UIConstraints t m, Ord n, Show n, Show a)
  => Map n (Event t a) -> VtyWidget t m (Dynamic t (Map n ()))
thingo tingMap = do
  rec tabNav <- tabNavigation
      let
        nav = leftmost [tabNav]
        tileCfg = def { _tileConfig_constraint = pure $ Constraint_Fixed 10}
        updates = leftmost [never]
      listOut <- runLayout (pure Orientation_Column) 10 nav $
        listHoldWithKey tingMap updates $  \k t -> tile tileCfg $ do
            click <- void <$> mouseDown V.BLeft
            pb <- getPostBuild
            let focusMe = leftmost [click, pb]
            r <- aBox (constant def) (k, t)
            return (focusMe, r)
  return listOut

aBox :: (UIConstraints t m, Show n, Show a)
  => Behavior t BoxStyle -> (n, Event t a) -> VtyWidget t m ()
aBox style (n, e) = col $ do
  dw <- displayWidth
  eb <- hold "Waiting..." $ toText <$> e
  _ <- fixed 50 $ row $ do
    fixed ((\x -> round $ fromIntegral x / (2 :: Float)) <$> dw) $ boxTitle style (toText n) $ scrollableText never eb
  return ()


data FormButtons = FormSubmit | FormCancel

form :: (UIConstraints t m) => VtyWidget t m ()
form = undefined {--do
  let submit = col $ do
        fixed 10 $ row $ do
          submit <- fixed 10 $ textButtonStatic def "Send"
          return $ FormSubmit <$ submit
  rec out <- networkHold button $ ffor (switch (current out)) $ \case
        Left FormSubmit -> do
          liftIO . putStrLn $ "A"
          return Just
        Right () -> text
  return ()--}
  

toText :: (Show a) => a -> T.Text
toText = T.pack . show

textWidget :: (Reflex t, Monad m, Show a) => a -> VtyWidget t m ()
textWidget = display . constant
