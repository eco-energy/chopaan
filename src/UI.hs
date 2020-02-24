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

import Node (NodeId(..), NodeS)

import Registry ( Kibbutz(..)
                , KibbutzEvents(..)
                , writeToPubQ
                , KMSensor
                , NodeT
                , KMState
                , KConnM
                , initKMS)

import qualified Data.Vector as Vec

import GHC.Generics (Generic)

import Control.Monad.Reader
import Lens.Micro.TH (makeLenses)

import qualified Proto.NodeMessages as NM

import qualified Data.Time.Clock as Time
import Data.ULID

import Transactor (mkETR)
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import StateMonitor (KibbutzMonitor, readKM)


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


drawMonitor :: (Show n) => Bool -> Int -> [(NodeT, n)] -> [(NodeT, Int)] -> Widget KibbutzUI
drawMonitor _ _ nms _ =
  B.borderWithLabel (withAttr titleAttr $ str "HH Monitor") $ drawNodeMetrics nms
  where
    drawNodeMetrics :: Show n => [(NodeT, n)] -> Widget KibbutzUI
    drawNodeMetrics (nm:nmx) = C.center $ foldl (<=>) (drawNodeMetric nm) $ map drawNodeMetric nmx
    drawNodeMetrics [] = C.center $ str "No Monitor Nodes Found!"
    drawNodeMetric :: Show n => (NodeT, n) -> Widget a
    drawNodeMetric (n, nm) =
      B.borderWithLabel (withAttr titleAttr $ renderNodeId n) $
          strWrap (show nm)
          -- <=>
          --strWrap ("connection count: " <> (show count))
    --merge :: [(a, b)] -> [(a, c)] -> [(a, b, c)]
    --merge ss ii = map (uncurry a') (zip ss ii) 
      --where
        --a' (n, ns) (n', ni) = (n, ns, ni)
        -- (drawSensor _sensors) <=> (drawPower _powerS) <=> (drawEnergy _energyS)



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
        EvKey (KEnter) [] -> M.continue . liftTransactor =<< (liftIO $ executeTransaction transactor kibbutz)
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
