{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module UI where

import Lens.Micro (Lens', (^.))
import Data.Maybe (maybeToList, fromMaybe)
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

import Registry (printQueueStream,
                 NodeT, Kibbutz(..), KibbutzEvents(..), KibbutzMonitor
                 -- effectful
                , getKibbutz
                , initKibbutzMonitor
                , updateKM
                , queueStream
                , writeToPubQ)

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
validateStakeListForTx ss = energyBalance == 0
  where
    energyBalance = sum $ map energyStake $ unStakeList ss

toTransaction :: [Stake] -> Transaction
toTransaction ss = Transaction $ map (\s-> (_stakingNode s, energyStake s)) ss

prepTx :: StakeList -> Time.NominalDiffTime -> IO ([(NodeT, NM.EnergyTransactionRequest)], Transaction)
prepTx sf leadTime = do
  txId <- (Text.pack . show) <$> getULID
  startTime <- Time.addUTCTime leadTime <$> Time.getCurrentTime
  let
    txReqs = map (\(n, et) -> (n, et txId startTime)) etrs
    tx = toTransaction stakes
  return (txReqs, tx)
  where
    etrs = map toETR stakes
    stakes = filter (_participating) $ unStakeList sf
    toETR :: Stake -> (NodeT, (Text.Text -> Time.UTCTime -> NM.EnergyTransactionRequest))
    toETR Stake {..} = (_stakingNode, msg)
      where
        msg = mkETR (abs _power) _duration dir
        dir = if (_power > 0) then NM.Outgoing else NM.Incoming

executeTransaction :: TransactorS -> Kibbutz -> IO TransactorS
executeTransaction t@TransactorS{..} Kibbutz{..} = if validateStakeListForTx txForms then exec else return t
  where
    exec = do
      (reqs, tx) <- prepTx txForms (60 * 2 :: Time.NominalDiffTime)
      _ <- (mapM (uncurry $ writeToPubQ outQueue) reqs)
      return $ mkTransactor nodes_t $ tx:transactions


initStake :: NodeT -> Stake
initStake n = Stake n False 0 0


stakeForm :: Int -> NodeT -> Stake -> StakeForm
stakeForm i n =
    let
      selQ = "Household?"
      hname = (unNodeId n)
      label s w = padBottom (T.Pad 1) $ (vLimit 1 $ hLimit 15 $ strWrap s <+> fill ' ') <+> w
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

drawTForms :: [NodeT] -> StakeList -> Bool -> Widget KibbutzUI
drawTForms ns fs focus = (L.renderList form focus fs) <+> C.hCenter help -- (form (head ns) (mkTForms ns $ (initStake $ head ns))) 
    where
      form :: Bool -> StakeForm -> Widget KibbutzUI
      form selected f = B.border $ padTop (T.Pad 1) $ hLimit 50 $ F.renderForm f
      forms (n:nx) (f:fx) = foldl (<+>) (form n f) (map (uncurry form) $ zip nx fx)
      forms [] [] = str ""
      help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
      body = strWrap $ "- Power is Watts in float. Positive for Outgoing, Negative for Incoming \n" <>
                       "- Duration is in Seconds  \n" <>
                       "- press (q) to exit"


drawTransactor :: Bool -> [NodeT] -> TransactorS -> Widget KibbutzUI
drawTransactor focus ns TransactorS {..} = B.borderWithLabel (withAttr titleAttr $ str "Transactor") $ drawTransactions <=> drawTransactionForm
  where
    drawTransactions = strWrap $ show transactions
    drawTransactionForm = drawTForms ns txForms focus

mkTransactor :: [NodeT] -> [Transaction] -> TransactorS
mkTransactor ns txs = TransactorS ns txs fs
  where
    fs = stakeList $ mkTForms ns $ map initStake ns

titleAttr :: A.AttrName
titleAttr = "title"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
    [ (B.borderAttr,         V.yellow `on` V.black)
    , (titleAttr,            fg V.cyan)
    ]

drawMonitor :: [(NodeT, NodeS)] -> Widget KibbutzUI
drawMonitor nms = B.borderWithLabel (withAttr titleAttr $ str "HH Monitor") $ drawNodeMetrics nms
  where
    drawNodeMetrics :: [(NodeT, NodeS)] -> Widget KibbutzUI
    drawNodeMetrics (nm:nmx) = C.center $ foldl (<=>) (drawNodeMetric nm) $ map drawNodeMetric nmx
    drawNodeMetrics [] = C.center $ str ""
    drawNodeMetric :: (NodeT, NodeS) -> Widget a
    drawNodeMetric (n, NodeMetrics {..}) = B.borderWithLabel (withAttr titleAttr $ renderNodeId n) $ ((drawPower _powerS) <=> (drawEnergy _energyS))
      where
        drawPower ps = strWrap $ show ps <> "\n"
        drawEnergy es = strWrap $ show es <> "\n"


-- write a metricsheet render function which can be <*>'d over 
drawKibbutz :: KibbutzState -> [Widget KibbutzUI]
drawKibbutz KibbutzState { kibbutz,  nodeStates, transactor } =
  [(drawMonitor nodeStates) <=> (drawTransactor True nodes transactor)]  
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
    T.AppEvent (StateUpdate) -> M.continue . (\ns -> s{nodeStates = ns}) =<< (liftIO $ monitorState kibbutz)
    T.VtyEvent vtype ->
      case vtype of
        EvKey (KChar 'q') [] -> M.halt s
        EvKey (KEnter) [] -> M.continue . liftTransactor =<< (liftIO $ executeTransaction transactor kibbutz)
        _ -> M.continue . liftTransactor =<< handleTransactorEvent transactor e
    _ -> M.continue s
    where
      liftTransactor = (\t-> s{transactor = t})


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

runTUI :: Kibbutz -> BChan KibbutzEvents -> IO ()
runTUI kbtz uiChan = do
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  initialState <- buildInitialState kbtz
  -- printQueueStream (queueStream . inQueue . kibbutz $ initialState)
  endState <- M.customMain initialVty buildVty (Just uiChan) kibbutzApp initialState
  return ()

data KibbutzState = KibbutzState
  { kibbutz :: Kibbutz
  , nodeStates :: [(NodeT, NodeS)]
  , transactor :: TransactorS
  , _focus :: Focus.FocusRing KibbutzUI
  }
  deriving (Generic)

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

buildInitialState :: Kibbutz -> IO KibbutzState
buildInitialState k = do
  print ("initMonitState")
  ms <- monitorState k
  print ("initTrx etc")
  let
    trxtr = mkTransactor (nodes k) []
    focusR = Focus.focusRing []
  return $ KibbutzState k ms trxtr focusR 

nodeStream :: Time.UTCTime -> Kibbutz -> NodeT -> SerialT IO NodeS
nodeStream initTime k n = runNodeMonitor initTime n $ queueStream $ inQueue k

initMonitorState :: Kibbutz -> IO [(NodeT, NodeS)]
initMonitorState = undefined

-- this should be a scan
monitorState :: Kibbutz -> t m (SMap.Map NodeT NodeS)
monitorState k@Kibbutz{..} = do
  initTime <- Time.getCurrentTime
  print ("initTime", initTime)
  let
    nS a = nodeStream initTime k a
  print ("getting ns")
  (x:xs) <- S.toList $ serially $ S.scanl' (id . id) nS $ S.fromList nodes
  print ("got ns")
  return $ zip nodes $ map (fromMaybe defNodeS) (x:xs) -- zip nlist (map () states)


-- the monadic action that is visualization must be S.mapM'd over it.
runMonitorVis :: (Foldable f, Monad m) => f NodeT -> t m (NodeT, NodeS) -> Widget KibbutzUI
runMonitorVis = undefined
