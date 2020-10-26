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
import Chopaan.UI.Transactor

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class (lift)
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

import Chopaan.Kibbutz.Transactor (Tx(..), TransactionStatus(..))

data Monitor = Monitor_State
             | Monitor_RuntimeStats
             | Monitor_Logs
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Dispatches = Dispatch_Transactions
                | Dispatch_NodeConfig
                | Dispatch_MeshConfig
  deriving (Show, Read, Eq, Ord, Enum, Bounded)


type EventMap t m n a = UIConstraints t m => Map n (Event t a)


monitor :: forall t m n a b c. (UIConstraints t m, Ord n, Show n, Show a, Show b, Show c)
  => EventMap t m n a -> EventMap t m n b -> EventMap t m n c -> Dynamic t (Tx n) -> Dynamic t TransactionStatus
  -> VtyWidget t m (Event t ())
monitor sensors runtimeStats logs txs txStatuses = do
  inp <- input
  let nodes = Map.keys sensors
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
        Left (Left Monitor_State) -> escapable $ tabMapView sensors
        Left (Left Monitor_RuntimeStats) -> escapable $ tabMapView runtimeStats
        Left (Left Monitor_Logs) -> escapable $ tabMapView logs
        Left (Right Dispatch_Transactions) -> escapable $ (transactor txs txStatuses)
        Left (Right Dispatch_NodeConfig) -> escapable $ form
        Left (Right Dispatch_MeshConfig) -> escapable $ form
        Right () -> buttons
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing


tabMapView :: (UIConstraints t m, Ord n, Show n, Show a)
  => Map n (Event t a) -> VtyWidget t m (Dynamic t (Map n ()))
tabMapView tingMap = do
  rec tabNav <- tabNavigation
      let
        nav = leftmost [tabNav]
        tileCfg = def { _tileConfig_constraint = pure $ Constraint_Min 30}
        updates = leftmost []
      listOut <- runLayout (pure Orientation_Row) 0 nav $ do
        listHoldWithKey tingMap updates $ \k v -> tile tileCfg $ do
          click <- void <$> mouseDown V.BLeft
          pb <- getPostBuild
          let focusMe = leftmost [click, pb]
          r <- (aBox (k, v))
          return (focusMe, r)
  return listOut

aBox :: (UIConstraints t m, Show n, Show a)
  => (n, Event t a) -> VtyWidget t m ()
aBox (n, e) = do
  f <- focus
  _ <- col $ do
    dw <- displayWidth
    dh <- displayHeight
    eb <- hold "Waiting..." $ toText <$> e
    fixed dh $ row $ do
      fixed dw $ boxTitle (current $ border <$> f) (toText n) $ richText (def) eb
  return ()
  where
    div' = (\x -> round $ fromIntegral x / (2 :: Float))
    border x = case x of
                 True -> doubleBoxStyle
                 False -> singleBoxStyle


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

{--
scrollableList
  :: forall t m a. (Reflex t, MonadHold t m, MonadFix m)
  => Event t Int
  -- number of elements to scroll by
  -> [VtyWidget t m a]
  -- list of widget elements
  -> VtyWidget t m (Behavior t (Int, Int))
scrollableList scrollBy ws = do
  dw <- displayWidth
  let imgs = wrap <$> (constant ws) <*> current dw
  kup <- key V.KUp
  kdown <- key V.KDown
  m <- mouseScroll
  let requestedScroll :: Event t Int
      requestedScroll = leftmost
        [ 1 <$ kdown
        , (-1) <$ kup
        , ffor m $ \case
            ScrollDirection_Up -> (-1)
            ScrollDirection_Down -> (1)
        , scrollBy
        ]
      updateLine maxN delta ix = min (max 0 (ix + delta)) maxN
  lineIndex :: Dynamic t Int <- foldDyn (\(maxN, delta) ix -> updateLine (maxN - 1) delta ix) 0 $
    attach (length <$> imgs) requestedScroll
  tellImages $ fmap ((:[]) . V.vertCat) $ drop <$> current lineIndex <*> imgs
  return $ (,) <$> ((+) <$> current lineIndex <*> pure 1) <*> (length <$> imgs)
  where
    wrap :: [VtyWidget t m a] -> Int -> [V.Image]
    wrap x maxWidth = concatMap (fmap)
--}
