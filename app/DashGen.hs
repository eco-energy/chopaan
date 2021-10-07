{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import GHC.Generics
import Grafana
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Bifunctor
import Data.List

import System.Directory
import qualified System.Envy as E
import Options.Applicative

import Control.Monad.IO.Class
import Chopaan.Types
import Chopaan.Graph hiding (GraphType, Mesh)
import Chopaan.Graph.Kbtz
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId
import Data.Influxable hiding (Query)
import qualified Streamly.Prelude as S


-- nodes :: [T.Text]
-- nodes = (T.pack . pure) <$> ['A'..'Z']

data GraphType = Power | Energy | Battery | Mesh
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

getQ :: NodeQueries -> GraphType -> [Query]
getQ qs d = (Influx . InfluxQuery . renderQuery) <$> q
  where
    q = case d of
      Power -> powerQ qs
      Energy -> energyQ qs
      Battery -> batteryQ qs
      Mesh -> meshQ qs

getUnit :: GraphType -> UnitFormat
getUnit d = let
  t = case d of
    Power -> "watts"
    Energy -> "watt seconds"
    Battery -> "watt seconds"
    Mesh -> "dB (RSSI)"
  in OtherFormat t

dashName :: GraphType -> T.Text
dashName = showText

nodeGraph :: NodeMAC -> GraphType -> [Query] -> Graph
nodeGraph n g qs = defaultGraph
  { graphTitle = (showText n) <> " " <> (showText g)
  , graphQueries = qs
  , graphNullPointMode = Connected
  , graphUnit = Just (getUnit g)
  }

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

nodePanel :: GraphType -> (NodeMAC -> NodeQueries) -> NodeMAC -> GridPos -> Panel
nodePanel g qs n = graphPanel (nodeGraph n g (getQ (qs n) g))

kbtzMeasurementDash :: KbtzName -> [NodeMAC] -> GraphType -> KbtzDash
kbtzMeasurementDash k ns g = KbtzDash g k d
  where
    d = defaultDashboard
      { dashboardIdentifier = Just (fromEnum g)
      , dashboardTitle = (unKbtzId k) <> " " <> (showText g) <> " Dashboard"
      , dashboardPanels = (uncurry (nodePanel g nq)) <$> (zip ns (gridLayout (length ns) nRows))
      , dashboardTime = TimeRange (Interval 360 Days) Nothing
      , dashboardRefresh = Interval 5 Seconds
      , dashboardVersion = 1
      }
    nq n = nodeQueries (asKbtzNode k n) 
    nRows = 3

data KbtzDash = KbtzDash
  { graphType :: GraphType
  , kbtz :: KbtzName
  , dashConfig :: Dashboard
  }

kbtzDir :: FilePath -> KbtzName -> FilePath
kbtzDir base k = base <> (T.unpack $ prefix
           <> (safe . unKbtzId $ k)
           <> "/")
  where
    safe = T.toLower . T.replace " " "_"
    prefix = case (last base) of
      '/' -> ""
      _ -> "/"

dashPath :: FilePath -> KbtzDash -> FilePath
dashPath base k = base <> (T.unpack $ prefix
           <> (safe . unKbtzId . kbtz $ k)
           <> "/"
           <> (safe . showText . graphType $ k)
           <> ".json")
  where
    safe = T.toLower . T.replace " " "_"
    prefix = case (last base) of
      '/' -> ""
      _ -> "/"


chopaanDashes :: [(KbtzName, [NodeMAC])] -> [KbtzDash]
chopaanDashes kns = (((uncurry kbtzMeasurementDash) <$> kns) <*> [minBound..maxBound])

writeDashes :: FilePath -> [KbtzDash] -> IO ()
writeDashes base dashes = mapM_
  (\d -> do
      BS.writeFile (dashPath base d) $ getDashboardJSON  $ dashConfig d)
  dashes
  
      
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
  let pollDBForStructure prevStruct = do
        kns <- runGraphM defPoolConf db $ withKbtzPool $ \c -> do
          ks <- sort <$> getKbtzim c
          liftIO $ print $ "found kbtzim: " <> (show ks)
          ns <- mapM (fmap sort . getKbtzNodes c) ks
          return $ zip ks ns
        liftIO $ print $ "found kbtzim: " <> (show kns)
        case (kns == prevStruct) of
          True -> do
            print $ "Going to Sleep, nothing changed"
            return prevStruct
          False -> do
            print ("Creating Directories")
            mapM_ (\k -> cd (kbtzDir output k)) (fst <$> kns)
            print $ "Writing Kbtzim: " <> (show $ length kns)
            let dashes = chopaanDashes kns
            writeDashes output dashes
            return $ kns
  S.drain $ S.delay (pollEveryMin 1)
    $ S.trace (liftIO . print)
    $ S.iterateM pollDBForStructure (pure [])
  where
    defPoolConf = PoolConf 1 1 1
    cd = createDirectoryIfMissing True
    structByLen :: [(KbtzName, [NodeMAC])] -> (Int, [Int])
    structByLen kns = (length kns, fmap length kns)
    pollEveryMin m = 60 * m
