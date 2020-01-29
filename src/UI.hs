{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module UI where

import Lens.Micro (Lens', (^?), (^.))
import Control.Monad (void)
import Data.Monoid
import Data.Maybe (fromJust, fromMaybe)
import qualified Graphics.Vty as V

import qualified Data.Text as Text
import qualified Data.Set as Set

import qualified Brick.Main as M
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Brick.Types (ViewportType(..), Padding(..),  Widget )
import Brick.Widgets.Core (viewport, strWrap, padTop, fill, padBottom, str,  (<+>), (<=>)
                          , vLimit
                          , hLimit
                          , vBox
                          , withAttr
                          , Named(..)
                          ) 
-- color layering fns
import Brick.Util (on, fg, bg)

-- dialog box
import Brick.Widgets.Dialog (dialog, renderDialog, handleDialogEvent)

-- progress bar for transaction
import Brick.Widgets.ProgressBar (progressBar)


-- ** UI Combinators
-- | Centering
import qualified Brick.Widgets.Center as C
-- | Bordering
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)

-- | List api
import Brick.Widgets.List (listSelectedAttr, List, GenericList(..), list, renderList, renderListWithIndex)

import Brick.BChan

import qualified Brick.Forms as F

import qualified Brick.Focus as Focus

import Graphics.Vty.Input.Events

import Node (NodeId(..), NodeS, NodeMetrics(..), runNodeMonitor, defNodeS)

import Registry (printQueueStream, NodeT, ThingName, getKibbutz, Kibbutz(..), queueStream, SubQueue, KibbutzEvents(..))

import Streamly hiding ((<=>))
import qualified Streamly.Prelude as S

import GHC.Generics (Generic)

import Control.Monad.Reader
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro.TH (makeLenses)

import Mqtt (defMQOpts, runMqtt)


data KibbutzUI = HHListUI | MonitorUI | TxListUI | TxFormUI TXFormField deriving (Eq, Ord, Show)

type TxNodeId = Int

data TXFormField = NodeField TxNodeId | ParticipatingField TxNodeId | PowerField TxNodeId | DurationField TxNodeId  deriving (Eq, Ord, Show)

newtype Transaction = Transaction { stakes :: [(NodeT, Double)] } deriving (Eq, Ord, Show, Generic)

type StakeForm = F.Form Stake KibbutzEvents KibbutzUI

type StakeList = List KibbutzUI StakeForm

data TransactorS = TransactorS
  { nodes_t :: Set.Set NodeT
  , transactions :: [Transaction]
  , txForms :: StakeList
  } deriving (Generic)

data Stake = Stake
  { _stakingNode :: NodeT
  , _participating :: Bool
  , _power :: Double
  , _duration :: Int
  } deriving (Eq, Ord, Show)


makeLenses ''Stake

stakeList :: [StakeForm] -> StakeList
stakeList xs = L.list TxListUI (Vec.fromList xs) 1 

initStakeList = stakeList []

addStake :: StakeList -> StakeForm -> StakeList
addStake xs x = L.listInsert 0 x xs


--executeTransaction :: StakeList -> IO ()
--executeTransaction xs = map toETR xs 

initStake :: NodeT -> Stake
initStake n = Stake n False 0 0


stakeForm :: Int -> NodeT -> Stake -> StakeForm
stakeForm i n =
    let
      selQ = "Select Household?"
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
drawTForms ns fs focus = (renderList form focus fs) <+> C.hCenter help -- (form (head ns) (mkTForms ns $ (initStake $ head ns))) 
    where
      form :: Bool -> StakeForm -> Widget KibbutzUI
      form selected f = B.border $ padTop (T.Pad 1) $ hLimit 50 $ F.renderForm f
      forms (n:nx) (f:fx) = foldl (<+>) (form n f) (map (uncurry form) $ zip nx fx)
      forms [] [] = str ""
      help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
      body = strWrap $ "- Power is a float \n" <>
                       "- Duration must be an integer (try entering an\n" <>
                       "  invalid duration!)\n" <>
                       "- Spacebar toggles direction\n" <>
                       "- (q) quit, mouse interacts with fields"


drawTransactor :: Bool -> [NodeT] -> TransactorS -> Widget KibbutzUI
drawTransactor focus ns TransactorS {..} = B.borderWithLabel (withAttr titleAttr $ str "Transactor") $ drawTransactions <=> drawTransactionForm
  where
    drawTransactions = strWrap $ show transactions
    drawTransactionForm = drawTForms ns txForms focus

mkTransactor :: Set.Set NodeT -> [Transaction] -> TransactorS
mkTransactor ns txs = TransactorS ns txs fs
  where
    fs = stakeList $ mkTForms nl sts
      where
        nl = Set.toList ns
        sts = map initStake nl

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
    drawNodeMetric :: (NodeT, NodeS) -> Widget a
    drawNodeMetric (n, NodeMetrics {..}) = B.borderWithLabel (withAttr titleAttr $ renderNodeId n) $ ((drawPower _powerS) <=> (drawEnergy _energyS))
      where
        drawPower ps = strWrap $ show ps <> "\n"
        drawEnergy es = strWrap $ show es <> "\n"


-- write a metricsheet render function which can be <*>'d over 
drawKibbutz :: KibbutzState -> [Widget KibbutzUI]
drawKibbutz KibbutzState { kibbutz,  nodeStates, transactor } =
  [(drawMonitor nodeStates) <=> (drawTransactor True (Set.toList nodes) transactor)]  
  where
    Kibbutz{..} = kibbutz

nodeList :: Set.Set NodeT -> List KibbutzUI NodeT
nodeList n = L.list HHListUI (Vec.fromList . Set.toList $ n) 1

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
        --EvKey (KEnter) [] -> M.continue =<< executeTransaction transactor
        _ -> M.continue . (\t-> s{transactor = t}) =<< handleTransactorEvent transactor e
    _ -> M.continue s

executeTransaction :: TransactorS -> IO TransactorS
executeTransaction = undefined 

appEvent :: s -> p -> T.EventM n (T.Next s)
appEvent l _ = M.continue l

listDrawElement :: Bool -> NodeT -> Widget KibbutzUI
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (strWrap $ "<" <> (Text.unpack . unNodeId $ s) <> ">")
                   else strWrap $ (Text.unpack . unNodeId $ s)
    in C.hCenter $ selStr a


customAttr :: A.AttrName
customAttr = listSelectedAttr <> "custom"


tui :: IO ()
tui = do
  eventChan <- newBChan 1000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  initialState <- buildInitialState thingTypeName
  (runMqtt defMQOpts (kibbutz initialState) eventChan)
  --printQueueStream (queueStream . inQueue . kibbutz $ initialState)
  endState <- M.customMain initialVty buildVty (Just eventChan) kibbutzApp initialState
  return ()
  where
    thingTypeName = "kibbutz-pilot-node"

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
    (_, form) = ((fromMaybe (0, (head . Vec.toList . listElements $ slist)) $ L.listSelectedElement slist))
    newf :: StakeForm -> T.EventM KibbutzUI StakeForm
    newf fm = F.handleFormEvent e fm
    {--
    T.VtyEvent vtype -> 
      case vtype of
        EvKey (KRight) [] -> undefined
        EvKey (KBegin) [] -> undefined
       _ -> return s --}

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []{--
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.magenta)
    ]--}


selectCursor :: KibbutzState -> [T.CursorLocation KibbutzUI] -> Maybe (T.CursorLocation KibbutzUI)
selectCursor s@KibbutzState{transactor} clocs = case (L.listSelectedElement txForms) of
  Nothing -> M.showFirstCursor s clocs
  Just (idx, _) -> Just $ clocs !! (min ((length clocs) - 1) (max 0 idx))
  where
    TransactorS{txForms} = transactor

kibbutzApp :: M.App KibbutzState KibbutzEvents KibbutzUI
kibbutzApp = M.App
  { appDraw = drawKibbutz
  , appChooseCursor = selectCursor
  , appHandleEvent = kibbutzEvent
  , appStartEvent = pure
  , appAttrMap = const theMap
  }

type KibbutzName = Text.Text

buildInitialState :: KibbutzName -> IO KibbutzState
buildInitialState thingTypeName = do
  k <- getKibbutz thingTypeName
  ms <- monitorState k
  return $ KibbutzState k ms (mkTransactor (nodes k) []) (Focus.focusRing [])

nodeStream :: Kibbutz -> NodeT -> IO (Maybe NodeS)
nodeStream k n = S.head $ runNodeMonitor n $ queueStream $ inQueue k

monitorState :: Kibbutz -> IO [(NodeT, NodeS)]
monitorState k@Kibbutz{..} = do
  let
    ns = nodes
    nlist = Set.toList ns
  states <- mapM (nodeStream k) nlist
  return $ zip nlist (map (fromMaybe defNodeS) states)
