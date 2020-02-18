{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module UI (runTUI, mkUIChan, refreshTick, prepTx, Stake(..)) where

import Lens.Micro (Lens', (^.))
import Data.Maybe (fromJust, maybeToList, fromMaybe)
import qualified Graphics.Vty as V

import qualified Data.Text as Text
import qualified Data.Set as Set

import qualified Brick.Main as M
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Brick.Types (Padding(..), Widget )
import Brick.Widgets.Core (strWrap, padTop, fill, padBottom, str, (<+>), (<=>), vLimit, hLimit, vBox, withAttr) 

-- color layering fns
import Brick.Util (on, fg)

import Brick.Widgets.Dialog (dialog, renderDialog, handleDialogEvent)

import Brick.Widgets.ProgressBar (progressBar)


-- ** UI Combinators
-- | Centering
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)

-- | List api
import Brick.BChan

import qualified Brick.Forms as F

import qualified Brick.Focus as Focus

import Graphics.Vty.Input.Events

import Node (NodeId(..), NodeS, NodeMetrics(..), runNodeMonitor, defNodeS)

import Registry (isTQEmpty, Kibbutz(..), KibbutzEvents(..)
                , writeToPubQ
                , getMonitorState)

import qualified Data.Vector as Vec

import Streamly hiding ((<=>))
import qualified Streamly.Prelude as S

import GHC.Generics (Generic)

import Control.Monad.Reader
import qualified Data.Set as Set
import Lens.Micro.TH (makeLenses)

import Mqtt (defMQOpts, runMqtt)

import qualified Proto.NodeMessages as NM

import qualified Data.Time.Clock as Time
import Data.ULID

import Transactor (mkETR)
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as Map
import StateMonitor (NodeT, KMState, KConnM, initKMS, readKM)


data KibbutzUI = HHListUI | MonitorUI | TxListUI | TxFormUI TXFormField deriving (Eq, Ord, Show)

type TxNodeId = Int

data TXFormField = NodeField TxNodeId | ParticipatingField TxNodeId | PowerField TxNodeId | DurationField TxNodeId  deriving (Eq, Ord, Show)

newtype Transaction = Transaction { stakes :: [(NodeT, Double)] } deriving (Eq, Ord, Show, Generic)

type StakeForm = F.Form Stake KibbutzEvents KibbutzUI

type StakeList = L.List KibbutzUI StakeForm

data TransactorS = TransactorS
  { nodes_t :: [NodeT]
  , transactions :: [Transaction]
  , txForms :: StakeList
  } deriving (Generic)

data Stake = Stake
  { _stakingNode :: NodeT
  , _participating :: Bool
  , _power :: Double
  , _duration :: Int
  } deriving (Eq, Ord, Show)


energyStake :: Stake -> Double

energyStake Stake {..} = _power * (fromIntegral _duration)

makeLenses ''Stake

stakeList :: [StakeForm] -> StakeList
stakeList xs = L.list TxListUI (Vec.fromList xs) 1 

unStakeList :: StakeList -> [Stake]
unStakeList s =  F.formState <$> (Vec.toList . L.listElements $ s)
initStakeList :: StakeList

initStakeList = stakeList []

addStake :: StakeList -> StakeForm -> StakeList
addStake xs x = L.listInsert 0 x xs

validateStakeListForTx :: StakeList -> Bool
validateStakeListForTx ss = energyBalance == 0 && powerBalance == 0
  where
    energyBalance = sum $ map energyStake $ unStakeList ss
    powerBalance = sum $ map _power $ unStakeList ss

toTransaction :: [Stake] -> Transaction
toTransaction ss = Transaction $ map (\s-> (_stakingNode s, energyStake s)) ss

prepTx :: [Stake] -> Time.NominalDiffTime -> IO ([(NodeT, NM.EnergyTransactionRequest)], Transaction)
prepTx sf leadTime = do
  txId <- (Text.pack . show) <$> getULID
  startTime <- Time.addUTCTime leadTime <$> Time.getCurrentTime
  let
    txReqs = map (\(n, et) -> (n, et txId startTime)) etrs
    tx = toTransaction stakes
  return (txReqs, tx)
  where
    etrs = map toETR stakes
    stakes = filter (_participating) sf
    toETR :: Stake -> (NodeT, (Text.Text -> Time.UTCTime -> NM.EnergyTransactionRequest))
    toETR Stake {..} = (_stakingNode, msg)
      where
        msg = mkETR (abs _power) _duration dir
        dir = if (_power > 0) then NM.Outgoing else NM.Incoming

executeTransaction :: TransactorS -> Kibbutz -> IO TransactorS
executeTransaction t@TransactorS{..} Kibbutz{..} = if validateStakeListForTx txForms then exec else return t
  where
    exec = do
      (reqs, tx) <- prepTx (unStakeList txForms) (60 * 2 :: Time.NominalDiffTime)
      _ <- (mapM (uncurry $ writeToPubQ outQueue) reqs)
      return $ mkTransactor nodes_t $ tx:transactions


initStake :: NodeT -> Stake
initStake n = Stake n False 0 0


stakeForm :: Int -> NodeT -> Stake -> StakeForm
stakeForm i n =
    let
      selQ = "Household?"
      hname = (unNodeId n)
      label s w = padBottom (T.Pad 1) $ (vLimit 2 $ hLimit 25 $ strWrap s <+> fill ' ') <+> w
    in F.newForm [ label selQ F.@@= F.checkboxField participating (TxFormUI (ParticipatingField i)) hname   
                 , label "Power" F.@@= F.editShowableField power (TxFormUI (PowerField i))
                 , label "Duration" F.@@= F.editShowableField duration (TxFormUI (DurationField i))
                 ]

mkTForms :: [NodeT] -> [Stake] -> [StakeForm]
mkTForms ns stakes = map (uncurry3 stakeForm) $ zip3 ids ns stakes
  where
    uncurry3 f (a, b, c) = f a b c
    ids = [1,2..]

renderNodeId :: NodeT -> Widget n
renderNodeId = strWrap . Text.unpack . unNodeId

drawTForms :: StakeList -> Bool -> Widget KibbutzUI
drawTForms fs focus = C.hCenter help <=> (L.renderList form focus fs)  -- (form (head ns) (mkTForms ns $ (initStake $ head ns))) 
    where
      form :: Bool -> StakeForm -> Widget KibbutzUI
      form selected f = B.border $ padTop (T.Pad 1) $ hLimit 50 $ F.renderForm f
      forms (n:nx) (f:fx) = foldl (<+>) (form n f) (map (uncurry form) $ zip nx fx)
      forms [] [] = str "No Nodes Found!"
      help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
      body = strWrap $ "- Power is Watts in float. Positive for Outgoing, Negative for Incoming \n" <>
                       "- Duration is in Seconds  \n" <>
                       "- press (q) to exit"


drawTransactor :: Bool -> TransactorS -> Widget KibbutzUI
drawTransactor focus TransactorS {..} = B.borderWithLabel (withAttr titleAttr $ str "Transactor") $ drawTransactions <=> drawTransactionForm
  where
    drawTransactions = strWrap $ show transactions
    drawTransactionForm = drawTForms txForms focus

mkTransactor :: [NodeT] -> [Transaction] -> TransactorS
mkTransactor ns txs = TransactorS ns txs fs
  where
    fs = stakeList $ mkTForms ns $ map initStake ns

titleAttr :: A.AttrName
titleAttr = "title"


drawMonitor :: Bool -> Int -> [(NodeT, NodeS)] -> [(NodeT, Int)] -> Widget KibbutzUI
drawMonitor emptyTQ msgCnt nms ncs =
  B.borderWithLabel (withAttr titleAttr $ str "HH Monitor") $ drawTQ <=> drawNodeMetrics (merge nms ncs)
  where
    drawTQ :: Widget KibbutzUI
    drawTQ = B.borderWithLabel (withAttr titleAttr $ str "Message Count") $ C.center $ (if emptyTQ
                                                                                        then str "Queue Empty"
                                                                                        else str "Not Empty") <=> (str $ show msgCnt) 
    drawNodeMetrics :: [(NodeT, NodeS, Int)] -> Widget KibbutzUI
    drawNodeMetrics (nm:nmx) = C.center $ foldl (<=>) (drawNodeMetric nm) $ map drawNodeMetric nmx
    drawNodeMetrics [] = C.center $ str "No Monitor Nodes Found!"
    drawNodeMetric :: (NodeT, NodeS, Int) -> Widget a
    drawNodeMetric (n, nm@NodeMetrics {..}, count) = B.borderWithLabel (withAttr titleAttr $ renderNodeId n) $ strWrap (show nm)  <=> strWrap ("connection count: " <> (show count))
      where
        drawSensor sr = strWrap $ show sr <> "\n\n\n"
        drawPower ps = strWrap $ show ps <> "\n\n\n"
        drawEnergy es = strWrap $ show es <> "\n\n\n"
    merge :: [(NodeT, NodeS)] -> [(NodeT, Int)] -> [(NodeT, NodeS, Int)]
    merge ss ii = map (uncurry a') (zip ss ii) 
      where
        a' (n, ns) (n', ni) = (n, ns, ni)
        -- (drawSensor _sensors) <=> (drawPower _powerS) <=> (drawEnergy _energyS)



-- write a metricsheet render function which can be <*>'d over 
drawKibbutz :: KibbutzState -> [Widget KibbutzUI]
drawKibbutz KibbutzState { kibbutz, transactor, currentNodeState, currentConnStates, queueEmpty, msgCount' } =
  [(drawMonitor queueEmpty msgCount' currentNodeState currentConnStates) <+> (drawTransactor True transactor)]  
  where
    Kibbutz{..} = kibbutz

nodeList :: [NodeT] -> L.List KibbutzUI NodeT
nodeList n = L.list HHListUI (Vec.fromList n) 1

drawList :: L.List KibbutzUI NodeT -> Widget KibbutzUI
drawList l = ui
  where
    label = str "Household " <+> cur <+> str " of " <+> total
    cur = case l^.(L.listSelectedL) of
      Nothing -> str "-"
      Just i -> str (show (i + 1))
    total = str $ show $ Vec.length $ l^.(L.listElementsL)
    box = B.borderWithLabel label $
      hLimit 25 $
      vLimit 15 $
      L.renderList listDrawElement True l
    ui = C.vCenter $ vBox [ C.hCenter box
                          , str " "
                          ]

-- We have a transactor event handler
kibbutzEvent :: KibbutzState -> T.BrickEvent KibbutzUI KibbutzEvents -> T.EventM KibbutzUI (T.Next (KibbutzState))
kibbutzEvent s@KibbutzState{..} e =
  case e of
    T.AppEvent (StateUpdate) ->
      (M.continue =<< (liftIO . stateU $ s))
    T.VtyEvent vtype ->
      case vtype of
        EvKey (KChar 'q') [] -> M.halt s
        EvKey (KEnter) [] -> M.continue . liftTransactor =<< (liftIO $ executeTransaction transactor kibbutz)
        _ -> M.continue . liftTransactor =<< handleTransactorEvent transactor e
    _ -> M.continue s
    where
      liftTransactor = (\t-> s{transactor = t})
      stateU :: KibbutzState -> IO (KibbutzState)
      stateU s@KibbutzState{..} = su <$> comb
        where
          su :: (Bool, (CurNodes, CurConns)) -> KibbutzState 
          su (etq, (ns, cns)) = s{queueEmpty = etq,
                        currentNodeState = ns,
                        currentConnStates = cns}
          comb :: IO (Bool, (CurNodes, CurConns))
          comb = ((,) <$> emp <*> nsu)
          emp :: IO Bool
          emp = (liftIO $ atomically $ isTQEmpty kibbutz)
          nsu :: IO (CurNodes, CurConns)
          nsu =  liftIO $ atomically $ getMonitorState nodeStates connStates (nodes $ kibbutz)

appEvent :: s -> p -> T.EventM n (T.Next s)
appEvent l _ = M.continue l

listDrawElement :: Bool -> NodeT -> Widget KibbutzUI
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (strWrap $ "<" <> (Text.unpack . unNodeId $ s) <> ">")
                   else strWrap $ (Text.unpack . unNodeId $ s)
    in C.hCenter $ selStr a

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

mkUIChan :: IO (BChan KibbutzEvents)
mkUIChan = newBChan 1000

refreshTick :: Int -> BChan KibbutzEvents -> IO ()
refreshTick d chan = do
  forever $
    writeBChan chan StateUpdate >> threadDelay d

runTUI :: Kibbutz -> KMState -> KConnM -> BChan KibbutzEvents -> IO ()
runTUI kbtz kmState kConnS uiChan = do
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  initialState <- buildInitialState kbtz kmState kConnS
  endState <- M.customMain initialVty buildVty (Just uiChan) kibbutzApp initialState
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


isFormEvent :: T.BrickEvent KibbutzUI e -> Bool
isFormEvent = undefined

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
    slist = (txForms s)
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
