module EnergyState where


import qualified Data.Time as Time
import qualified Data.Map.Strict as Map

-- Vis
import qualified Text.PrettyPrint.Tabulate as PPT
import GHC.Generics (Generic)
import Data.Data


import Registry (NodeId(..))
----------------------------------------------------------------------------------
-- Metric Tracking

type WattSeconds = Double

type Watts = Double

type S = Time.DiffTime

data Audit = Audit
  { transmittedIn :: WattSeconds
  , transmittedOut :: WattSeconds
  , consumed :: WattSeconds
  , generated :: WattSeconds
  , tDiff :: S
  } deriving (Eq, Show, Ord, Generic, Data)

processES :: (S -> EnergyState -> a) -> S -> EnergyState -> a
processES f prevTime es = f prevTime es


{--
instance Semigroup Audit where
  m0 <> m1 = Audit tIn tOut c g newTimeDiff
    where
      [tIn, tOut, c, g, newTimeDiff] = map (\t -> (sum $ map t ms)) ts
      ms = [m0, m1]
      ts = [transmittedIn, transmittedOut, consumed, generated, tDiff]

instance Monoid Audit where
  mempty = Audit 0 0 0 0 0
--}

instance PPT.Tabulate Audit PPT.ExpandWhenNested


esToAudit :: S -> EnergyState -> Audit
esToAudit prevTime es = Audit tIn tOut c g tdiff
  where
    tIn :: WattSeconds
    tIn = es ^. batteryVoltage * es ^. gridToBatteryCurrent 
    tOut :: WattSeconds
    tOut = es ^. batteryVoltage * es ^. batteryToGridCurrent
    c :: WattSeconds
    c = es ^. batteryVoltage * es ^. batteryToLoadCurrent
    g :: WattSeconds
    g = es ^. batteryVoltage * es ^. solarInputCurrent * timeInSeconds
    timeInSeconds = (fromIntegral $ es ^. cpuTime) / (1000 * 60) -- milliSToSeconds
    tdiff = timeInSeconds - prevTime


newtype KibbutzState = KibbutzState { unKibbutzState :: Map.Map NodeId Audit } deriving (Show, Generic, Data)

instance PPT.CellValueFormatter NodeId


updateNodeState :: KibbutzState -> NodeId -> EnergyState -> KibbutzState
updateNodeState ns n es = KibbutzState $ Map.adjust updateMetric n ns'
  where
    ns' = unKibbutzState ns
    updateMetric = (<> processES s es)
    s = tDiff $ ns' Map.! n

printAudit :: KibbutzState -> IO ()
printAudit ns = PPT.printTable $ unKibbutzState ns

initMonitorState :: [ThingName] -> KibbutzState
initMonitorState ts = KibbutzState $ Map.fromList [((NodeId t), mempty) | t <- ts]
