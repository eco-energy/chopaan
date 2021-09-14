{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import GHC.Generics
import Grafana
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Bifunctor

import qualified System.Envy as E
import Options.Applicative

import Chopaan.Types
import Chopaan.Graph
import Chopaan.Graph.Kbtz
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId
import Data.Influxable


-- nodes :: [T.Text]
-- nodes = (T.pack . pure) <$> ['A'..'Z']

data DashType = Power | Energy | Battery | Mesh

getQ :: NodeQueries -> DashType -> [Query]
getQ qs d = case d of
  Power -> powerQ qs
  Energy -> energyQ qs
  Battery -> batteryQ qs
  Mesh -> meshQ qs
  

nodeGraph :: [Query] -> KbtzNode -> Graph
nodeGraph qs n = defaultGraph
  { graphTitle = nodeName n
  , graphQueries = serializeQuery <$> qs
  , graphNullPointMode = Connected
  , graphUnit = Just . OtherFormat $ "watts"
  }

nodeGraphs :: KbtzNode -> [Graph]
nodeGraphs = k nodeQueries

nodePanel :: T.Text -> GridPos -> Panel
nodePanel = graphPanel . nodeGraph

moveGridPos :: Int -> Int -> GridPos -> GridPos
moveGridPos dx dy p = p
  { panelXPosition = (panelXPosition p) + dx
  , panelYPosition = (panelYPosition p) + dy
  } 

gridLayout :: Int -> Int -> [GridPos]
gridLayout numberOfPanels numberOfRows = let
  panelW = floor (realToFrac maxDashboardWidth / realToFrac numberOfRows)
  panelH = panelW - 2
  x0 = 0
  y0 = 0
  initPos = GridPos panelW panelH x0 y0
  in [moveGridPos (panelW * (mod i numberOfRows)) (panelH * (div i numberOfRows)) initPos
     | i <- [0..numberOfPanels]
     ]

kbtzDash :: DashType -> [KbtzNode] -> Dashboard
kbtzDash k ns = defaultDashboard
  { dashboardIdentifier = Just 1
  , dashboardTitle = k <> " Dashboard"
  , dashboardPanels = nodePanel <$> ns <*> (gridLayout (length ns) nRows)
  , dashboardTime = TimeRange (Interval 360 Days) Nothing
  , dashboardRefresh = Interval 5 Seconds
  , dashboardVersion = 1
  }
  where
    nRows = 3

chopaanDash :: [(KbtzName, [NodeMAC])] -> [Dashboard]
chopaanDash = fmap (uncurry kbtzDash . bimap unKbtzId (fmap unNodeId))


writeDash :: FilePath -> Dashboard -> IO ()
writeDash base dash = BS.writeFile (base <> dashPath) $ getDashboardJSON dash
  where
    dashPath = T.unpack $ prefix
               <> (T.toLower . T.replace " " "_" $ dashboardTitle dash)
               <> ".json"
    prefix = case (last base) of
      '/' -> ""
      _ -> "/"
      
data DashOpts = DashOpts
  { dbOpts :: TinkerConf
  , outputPath :: FilePath
  } deriving (Generic)

dashParser :: Parser DashOpts
dashParser = DashOpts
  <$> tkParser
  <*> strOption (long "outpath" <> metavar "DASHBOARDFOLDER")

dashOpts :: ParserInfo DashOpts 
dashOpts = info (dashParser <**> helper) $
  fullDesc
  <> progDesc "Generates grafana dashboard configurations for all kibbutzim in the database"
  <> header "Generate Dashboards"


main :: IO ()
main = do
  (DashOpts db output) <- execParser dashOpts
  kns <- runGraphM defPoolConf db $ withKbtzPool $ \c -> do
    ks <- getKbtzim c
    ns <- mapM (getKbtzNodes c) ks
    return $ zip ks ns
  sequence_ $ writeDash output <$> (chopaanDash kns) 
  where
    defPoolConf = PoolConf 1 1 1
