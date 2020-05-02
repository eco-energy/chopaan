{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module UI (runTUI, mkUIChan, prepTx, Stake(..), genTick) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified Data.Time.Clock as Time

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Brick.Types (Padding(..), Widget )
import Brick.Widgets.Core (strWrap, padTop, str, (<+>), (<=>), hLimit, withAttr)
import qualified Brick.Widgets.Core as C 
import qualified Brick.Widgets.Center as C
import Brick.BChan
import qualified Brick.Forms as F
import qualified Brick.Focus as Focus
import Graphics.Vty.Input.Events


import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import GHC.Generics (Generic)


import Node (NodeId(..), NodeS)
import Transactor
import Registry (Kibbutz(..), KibbutzEvents(..), NodeT, KMState, KConnM)
import UI.Types
import StateMonitor (readKM)



renderNodeId :: NodeT -> Widget n
renderNodeId = strWrap . Text.unpack . unNodeId

drawTForms :: StakeList -> Bool -> Widget KibbutzUI
drawTForms fs focus = C.hCenter help <=> (L.renderList form focus fs)  -- (form (head ns) (mkTForms ns $ (initStake $ head ns))) 
    where
      form :: Bool -> StakeForm -> Widget KibbutzUI
      form _ f = B.border $ padTop (T.Pad 1) $ hLimit 50 $ F.renderForm f
      help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
      body = strWrap $ "- Power is Watts in float. Positive for Outgoing, Negative for Incoming \n" <>
                       "- Duration is in Seconds  \n" <>
                       "- press (q) to exit"


drawTransactor :: Bool -> TransactorS -> Widget KibbutzUI
drawTransactor focus TransactorS {..} = B.borderWithLabel (withAttr titleAttr $ str "Transactor") $ drawTransactions <=> drawTransactionForm
  where
    drawTransactions = strWrap $ show transactions
    drawTransactionForm = drawTForms txForms focus


titleAttr :: A.AttrName
titleAttr = "title"


drawMonitor :: (Show n) => Bool -> Int -> [(NodeT, n)] -> [(NodeT, Int)] -> Widget KibbutzUI
drawMonitor _ _ nms _ =
  C.vLimitPercent 100 $ B.borderWithLabel (withAttr titleAttr $ str "HH Monitor") $ drawNodeMetrics nms
  where
    drawNodeMetrics :: Show n => [(NodeT, n)] -> Widget KibbutzUI
    drawNodeMetrics (n:nm) = (C.hLimitPercent 50) . (C.vLimitPercent 100) $ C.vBox $ map (drawNodeMetric prop) (n:nm)
      where
        prop = round ((100 :: Float) / (fromIntegral $ (length nm) + 1))
    drawNodeMetrics [] = C.center $ str "No Monitor Nodes Found!"
    drawNodeMetric :: Show n => Int -> (NodeT, n) -> Widget a
    drawNodeMetric p (n, nm) =
      C.vLimitPercent p  $ B.borderWithLabel (withAttr titleAttr $ renderNodeId n) $
          strWrap $ show nm



-- write a metricsheet render function which can be <*>'d over 
drawKibbutz :: KibbutzState -> [Widget KibbutzUI]
drawKibbutz KibbutzState { transactor, currentNodeState, currentConnStates, queueEmpty, msgCount' } =
  [(drawMonitor queueEmpty msgCount' currentNodeState currentConnStates) <+> (drawTransactor True transactor)] 


-- We have a transactor event handler
kibbutzEvent :: KibbutzState -> T.BrickEvent KibbutzUI KibbutzEvents -> T.EventM KibbutzUI (T.Next (KibbutzState))
kibbutzEvent s@KibbutzState{..} e =
  case e of
    T.AppEvent (StateUpdate) ->
      (M.continue =<< (liftIO . stateU $ s))
    T.VtyEvent vtype ->
      case vtype of
        EvKey (KChar 'q') [] -> M.halt s
        EvKey (KEnter) [] -> M.continue . liftTransactor =<< (liftIO $ executeTransaction transactor $ outQueue kibbutz)
        _ -> M.continue . liftTransactor =<< handleTransactorEvent transactor e
    _ -> M.continue s
    where
      liftTransactor = (\t-> s{transactor = t})
      stateU :: KibbutzState -> IO (KibbutzState)
      stateU s' = su <$> comb
        where
          su :: (Bool, CurNodes) -> KibbutzState 
          su (etq, ns) = s'{queueEmpty = etq,
                        currentNodeState = ns}
          comb :: IO (Bool, CurNodes)
          comb = ((,) <$> emp <*> nsu)
          emp :: IO Bool
          emp = (liftIO $ return False)
          nsu :: IO (CurNodes)
          nsu =  liftIO $ atomically $ readKM nodeStates (nodes $ kibbutz)


mkUIChan :: IO (BChan KibbutzEvents)
mkUIChan = newBChan 1000

genTick :: BChan KibbutzEvents -> IO ()
genTick c = writeBChan c StateUpdate

runTUI :: Kibbutz -> KMState -> KConnM -> BChan KibbutzEvents -> IO ()
runTUI kbtz kmState kConnS uiChan = do
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  initialState <- buildInitialState kbtz kmState kConnS
  _ <- M.customMain initialVty buildVty (Just uiChan) kibbutzApp initialState
  return ()

type CurNodes = [(NodeT, NodeS)]
type CurConns = [(NodeT, Int)]

data KibbutzState = KibbutzState
  { kibbutz :: Kibbutz
  , nodeStates :: KMState
  , connStates :: KConnM
  , currentNodeState :: CurNodes 
  , currentConnStates :: CurConns
  , transactor :: TransactorS
  , _focus :: Focus.FocusRing KibbutzUI
  , kbtzTime :: Time.UTCTime
  , queueEmpty :: Bool
  , msgCount' :: Int
  }
  deriving (Generic)

buildInitialState :: Kibbutz -> KMState -> KConnM -> IO KibbutzState
buildInitialState k kmState kConnS = do
  initTime <- Time.getCurrentTime
  crntNS <- atomically $ readKM kmState (nodes k)
  crntConn <- atomically $ readKM kConnS (nodes k)
  let
    trxtr = mkTransactor (nodes k) []
    focusR = Focus.focusRing []
  return $ KibbutzState k kmState kConnS crntNS crntConn trxtr focusR initTime True 0 


isListEvent :: T.BrickEvent KibbutzUI e -> Bool
isListEvent e =
  case e of
    T.VtyEvent vtype ->
      case vtype of
        EvKey KUp [] -> True
        EvKey KDown [] -> True
        EvKey KHome [] -> True
        EvKey KEnd [] -> True
        EvKey KPageDown [] -> True
        EvKey KPageUp [] -> True
        _ -> False
    _ -> False

handleTransactorEvent :: TransactorS -> T.BrickEvent KibbutzUI KibbutzEvents -> T.EventM KibbutzUI TransactorS
handleTransactorEvent s e
      | isListEvent e = (\(T.VtyEvent vtype) -> liftToForm $ L.handleListEvent vtype slist) e
      | otherwise = do
          f <- newf form
          let
            newslist = L.listModify (const f) slist
          return $ (\k-> s{txForms=k}) newslist
  where
    liftToForm = liftM (\k-> s{txForms=k})
    slist = txForms s
    (_, form) = ((fromMaybe (0, (head . Vec.toList . L.listElements $ slist)) $ L.listSelectedElement slist))
    newf :: StakeForm -> T.EventM KibbutzUI StakeForm
    newf fm = F.handleFormEvent e fm


theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []{--
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.magenta)
    ]--}


selectCursor :: KibbutzState -> [T.CursorLocation KibbutzUI] -> Maybe (T.CursorLocation KibbutzUI)
selectCursor s@KibbutzState{transactor} clocs = case (L.listSelectedElement txForms) of
  Nothing -> M.showFirstCursor s clocs
  Just (idx, _) -> safeIdx idx
  where
    TransactorS{txForms} = transactor
    safeIdx idx = Just $ clocs !! (min ((length clocs) - 1) (max 0 idx))

kibbutzApp :: M.App KibbutzState KibbutzEvents KibbutzUI
kibbutzApp = M.App
  { appDraw = drawKibbutz
  , appChooseCursor = selectCursor
  , appHandleEvent = kibbutzEvent
  , appStartEvent = pure
  , appAttrMap = const theMap
  }

type KibbutzName = Text.Text
