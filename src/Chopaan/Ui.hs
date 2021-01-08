{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications, FlexibleContexts, NamedFieldPuns, InstanceSigs #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE ApplicativeDo, QuasiQuotes, DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs, RankNTypes, ConstraintKinds #-}
module Chopaan.Ui (mon, defGrid) where

import GHC.Generics hiding (R)

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Tree
import qualified Data.Foldable as F

import           Network.Wai.Handler.Warp (run)
import           Network.WebSockets (Connection, sendTextData)
import           Servant ( Get, Handler, Capture, Proxy(..), (:<|>)(..), (:>)
                         , serve, FromHttpApiData(..))
import           Servant.API.WebSocket (WebSocket)
import           Servant.HTML.Blaze (HTML)

import Chopaan.Kibbutz.Kibbutz (Kbtz(..), getNodes, KbtzId, scanKbtz, traceKbtz, runKbtz
                               , nodes, streams)
import Chopaan.Kibbutz.Transactor (Tx(..), TxPlan, TransactionStatus(..), Stake(..))
import Chopaan.Comm.Comm (Address)

import Chopaan.Node.Node (SensorS)
import Chopaan.Node.NodeId (NodeId)
import Chopaan.Node.Folds
import Proto.NodeMessageSchema.NodeMessages (RuntimeStats)

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Diagrams.Backend.SVG (B)
import qualified Diagrams.Backend.SVG as SVG
import Diagrams.Prelude hiding (render)
import Diagrams.TwoD.Layout.Grid
import Diagrams.TwoD.Layout.Tree

import Web.Suavemente
import Web.Suavemente.Diagrams

import Streamly

import qualified Streamly.Prelude as S

import Text.InterpolatedString.Perl6
import Text.Blaze (preEscapedString, Markup)
import Text.Blaze.Renderer.String (renderMarkup)

import ConCat.Misc
import Control.Monad.Trans.Reader

import Algebra.Graph.Labelled.AdjacencyMap



-- Existentialized Kbtz
data SomeKbtz t m n where
  Grid :: Kbtz t m n SensorS -> SomeKbtz t m n
  Mesh :: Kbtz t m n RuntimeStats -> SomeKbtz t m n
  Market :: Kbtz t m (TxPlan n) TransactionStatus -> SomeKbtz t m n
  Game :: Kbtz t m n R -> SomeKbtz t m' n -> (SomeKbtz t m n -> a) -> SomeKbtz t m (n, a)

pageMap ::
  (IsStream t, MonadAsync m, Address n, IsName n, FromHttpApiData n)
  => [SomeKbtz t m n]
  -> M.Map Page (SomeKbtz t m n)
pageMap pages = M.fromList $ fmap (\x -> (pMap x, x)) pages
                    where
                      pMap (Grid _) = GridP
                      pMap (Mesh _) = MeshP
                      pmap (Market _) = TxP

mon :: forall t m n.
  ( IsStream t
  , MonadAsync m
  , Address n
  , IsName n
  , FromHttpApiData n
  , Ord n
  , Show n
  )
  => (forall x. m x -> IO x)
  -> GridL n
  -> Kbtz t m n SensorS
  -> Kbtz t m n RuntimeStats
  -- -> Kbtz t m (TxPlan n) TransactionStatus
  -> m ()
mon hoister (GridL{nodesL}) grid mesh =
  liftIO $ serveKbtzm hoister (pageMap [Grid grid, Mesh mesh]) nodesL


changeNamed ::
  forall n a. (IsName n, Show a)
  => (n -> a -> Subdiagram B V2 Double Any -> Diagram B -> Diagram B)
  -> Diagram B
  -> M.Map n a
  -> Diagram B
changeNamed change d ss = foldl (\d' (n, s) -> withName n (change n s) d') d (M.toList ss)


changeES n s = atop . place (mkNode n s) . location


data GridL n = GridL
  { nodesL :: [n]
  , edgesL :: [(n, n)]
  } deriving (Eq, Ord, Show, Generic)


inOrder :: [n] -> GridL n
inOrder [] = GridL [] []
inOrder (n:ns) = GridL (n:ns) $ zip (n:ns) ns 


defGrid :: [n] -> GridL n
defGrid n = inOrder n

drawGrid :: forall n. NodeKey n => GridL n -> Diagram B
drawGrid GridL{nodesL, edgesL} = let
  dia = gridSnake (drawNode <$> nodesL)
  in dia --applyAll [connectOutside i j | (i, j) <- edgesL] dia    
  where
    drawNode :: n  -> Diagram B
    drawNode n = SVG.svgId (show n) $ (square 10
                                       # fc white
                                       # lc black
                                       # named n)

radialTree :: forall n a. (NodeKey n, Show a) => Tree (n, a) -> Diagram B
radialTree t =
   renderTree (\n -> (text (show n) # fontSizeG 0.5
                            <> circle 0.5 # fc white))
             (~~) (radialLayout t)
   # centerXY # pad 1.1

mkNode :: (Address n, Show a) => n -> a -> Diagram B
mkNode n s = SVG.svgId ("node-state-" <> show n) $ text (show s) # fontSizeL 0.2 # fc white

data Page = GridP | MeshP | TxP deriving (Eq, Ord)

instance Show Page where
  show (GridP) = "grid"
  show (MeshP) = "mesh"
  show (TxP) = "tx"



  
instance FromHttpApiData Page where
  parseUrlPiece :: T.Text -> Either T.Text Page 
  parseUrlPiece = parseUrlPiece


pageWithSockets :: (NodeKey n) => Double -> [n] -> Diagram B -> Page -> Markup
pageWithSockets w ns diag res = preEscapedString $
                        (markupRender $ emptyStyle [])
                        <> (webSocketScript ns $ show res)
                        <> (markupRender $ sendDiagram w diag)

type PageT m n = (ReaderT (Double, n) m Markup)

runPage :: n -> PageT m n -> m (Markup)
runPage n p = runReaderT p (widgetWidth, n)
  where
    widgetWidth = 1000
    widgetExtent = (250, 250)
    widgetCenterRow w m = xx <$> [0,1..m]
      where
        xx i = ((mod c m), c, c)
          where
            c = w * i

graphPage :: (MonadAsync m, NodeKey n) => PageT m [n]
graphPage = do
  (w, ns) <- ask
  return $ pageWithSockets w ns (drawGrid $ inOrder ns) GridP
  
treePage :: (MonadAsync m, NodeKey n, Show a) => PageT m (Tree (n, a))
treePage = do
  (w, t) <- ask
  return $ pageWithSockets w (F.toList $ fmap fst t) (radialTree t) MeshP
  
emptyStyle :: [a] -> Markup
emptyStyle [] = [q|
  <style>
  </style>|]
emptyStyle (n:ns) = [qc|
  <style>
  </style>|]


markupRender markup = [qc|
  <div id="result">{renderMarkup markup}</div>
  <table>
  |]
  
webSocketScript nodes res = [q|
  <script>
    const createNodeSocket = (node) => {
        const keepAlive = () => {
            ws.send(JSON.stringify({}));
            setTimeout(keepAlive, 1000);
        };

        let svgPath = "node-state-" + node;
        let wsPath = "node/" + "|] <> res <> [q|" + "/" + node;

        let ws = new WebSocket("ws://localhost:8080/" +  wsPath);
        
        ws.onopen = e => keepAlive();
        ws.onmessage = e => document.getElementById(svgPath).innerHTML = e.data;
        //ws.onclose = e => document.getElementById(svgPath).innerHTML =;
    };
    |] <> [qc|
    const ns = {asJSList $ show <$> nodes}
    ns.map(createNodeSocket);
  </script>
  |]
    
asJSList :: [String] -> String
asJSList ns = "[" <> (conv ns) <> "];"
  where
    conv [] = ""
    conv (x:xs) = foldl (\a b -> a <> ", " <> (safeShow b)) (safeShow x) xs
      where
        safeShow [] = error "empty string shouldn't happen on a safeShow!"
        safeShow (ss) = take (length ss) ss

wsClosed :: String
wsClosed = renderMarkup [q|
             "No Websocket Connection"
           |]

type MarkupAPI = "page" :> (Capture "resource" String) :> Get '[HTML] Markup
  
type API n = MarkupAPI :<|> (WebSocketAPI n)

type WebSocketAPI n = "node" :> (Capture "resource" String) :> (Capture "nodeid" n) :> WebSocket


renderGr :: (NodeKey n) => Page -> Double -> GridL n -> Markup
renderGr p w g@(GridL{nodesL}) = pageWithSockets w nodesL (drawGrid g) p

renderTr :: (NodeKey n, Show a) => Double -> Tree (n, a) -> Markup
renderTr w t = sendDiagram w $ radialTree t 

class HasPage a where
  page' :: (Page -> a -> Markup)

gridPage w ns = renderGr GridP w (inOrder ns)

meshPage w ns = renderGr MeshP w (inOrder ns)

--txnPage w ns = renderPage w renderGr (inOrder ns)

pageHandler :: (NodeKey n) => Double -> [n] -> Page -> Handler Markup
pageHandler w n p = case p of
  GridP -> pure $ gridPage w n
  MeshP -> pure $ meshPage w n
  --TxP -> pure $ txnPage w n


socketHandler :: forall t m n. (IsStream t, MonadAsync m, NodeKey n)
  => (forall x. m x -> IO x)
  -> M.Map Page (SomeKbtz t m n)
  -> n
  -> Page
  -> Connection
  -> Handler ()
socketHandler hoister pages node page conn = do
  case pages M.! page of
    (Grid g) -> c g
    (Mesh m) -> c m
    (Market t) -> d t
  where
    d :: (MonadAsync m) => Kbtz t m (TxPlan n) TransactionStatus -> Handler ()
    d = liftIO . hoister . sendtx
    c :: forall a. Show a => Kbtz t m n a -> Handler ()
    c = liftIO . hoister . sendKbtz
    sendtx :: Kbtz t m (TxPlan n) TransactionStatus -> m ()
    sendtx k = do
      (_ :: ()) <- sequence_ $ fmap f n 
      (_ :: ()) <- sequence_ $ fmap (S.drain . adapt) $ fmap (fmap g) s
      return ()
      where
        n :: [TxPlan n]
        n = nodes k
        s :: [t m TransactionStatus]
        s = streams k
        g :: TransactionStatus ->  m ()
        g x =  sendNode @m @n @TransactionStatus conn nodeSize node x 
        f :: TxPlan n -> m ()
        f (Tx tx)  = case M.lookup node tx of
          Nothing -> return ()
          (Just (Stake stake)) -> (liftIO
                                . sendTextData conn
                                . B.pack
                                . renderMarkup
                                . sendDiagram nodeSize
                                $ mkNode node stake)
             
    sendKbtz :: forall a. Show a => Kbtz t m n a -> m ()
    sendKbtz k = do
      case M.lookup node $ unKibbutz k of
        Nothing -> return ()
        (Just nodeS) ->
          S.mapM_ (liftIO
                   . sendTextData conn
                   . B.pack
                   . renderMarkup
                   . sendDiagram nodeSize
                   . mkNode node)
          $ adapt nodeS
    nodeSize = 10


sendNode :: forall m n a. (NodeKey n, MonadAsync m, Show a)
  => Connection
  -> Double
  -> n
  -> a
  -> m ()
sendNode conn nodeSize node payload = (liftIO
             . sendTextData conn
             . B.pack
             . renderMarkup
             . sendDiagram nodeSize
             $ mkNode node payload)

type UIConn t m n = (IsStream t, MonadAsync m, NodeKey n)

serveKbtzm :: forall t m n.
  UIConn t m n
  => (forall x. m x -> IO x)
  -> M.Map Page (SomeKbtz t m n)
  -> [n]
  -> IO ()
serveKbtzm hoister pageMap nodes = do
  putStrLn $ "Start Serving..."
  run 8080
    . serve (Proxy @(API n))
    $ (pg :<|> sc)
    where
      page "grid" = GridP
      page "mesh" = MeshP
      page "tx" = TxP
      page _ = error "Wrong Page!"
      sc :: String -> n -> Connection -> Handler ()
      sc s = flip (socketHandler hoister pageMap) (page s)
      pg :: String -> Handler Markup
      pg p = pageHandler 1000 nodes (page p)
      defaultHandler :: Handler (Markup)
      defaultHandler = return [q|
                                <div>
                                <text>You Need to Know Where You're Going</text>
                                <text></text>
                                </div>
                                |]
    
{--
data GridEvent a = StartCharge a
                 | StopCharge a
                 | StartDischarge a
                 | StopDischarge a
                 | BatteryLow a
                 | BatteryHigh a
                 | TxHigh a
                 | TxLow a
                 deriving (Eq, Ord, Show, Generic, Functor)
--}

type NodeKey n = (Address n, IsName n, FromHttpApiData n)


data CommSchema c = CommSchema
  { gridChannel :: c
  , meshChannel :: c
  , txChannel :: c
  } deriving (Eq, Ord, Show, Generic)

{--
data Kibbutzim

kibbutz :: forall t m n. (MonadAsync m, NodeKey n)
  => KbtzId
  -> (KbtzId -> m [n])
  -> (forall x. m x -> IO x)
  -> m ()
kibbutz kbtzId getNodes hoister = do
  ns <- getNodes kbtzId
  grid <- (traceKbtz monitor) . (traceKbtz save)
            . (scanKbtz sensorFold)
          =<< subscribeKbtz gridChannel ns
  mesh <- (traceKbtz monitor . traceKbtz save . scanKbtz connectivityTreeFold)
          =<< subscribeKbtz meshChannel ns
  (txplan, txmonitor, price) <- scanKbtz ((,,)
                                      <$> txPlanFold
                                      <*> txExecuteFold
                                      <*> pricingFold) grid
  liftIO $ serveKbtzm hoister (pages grid mesh (txplan, txmonitor, price)) ns
  where
    pages g m (pl, ex, pr) = pageMap [Grid g, Mesh m]
    subscribeKbtz :: forall t a. (IsStream t) => [n] -> m (Kbtz t m n a)
    subscribeKbtz ns = undefined


monitor :: forall a. a -> IO ()
monitor = undefined

save :: forall a. a -> IO ()
save = undefined
--}
