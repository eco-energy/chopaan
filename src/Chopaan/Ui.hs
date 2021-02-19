module Chopaan.Ui where

import Shpadoinkle
import Shpadoinkle.Html

import Diagrams
import qualified Diagrams.Prelude as D

import Streamly
import qualified Streamly.Prelude as S 


type R = Double

--newtype N a = N a

newtype E a  = E a

pie :: [N R] -> [(N R, N R, R)] -> R
pie nodes edges = undefined --treeLayout



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
