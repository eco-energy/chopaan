module Chopaan.UI.Tx where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
--import Control.Monad.NodeId

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe


import Graphics.Vty as V
import Reflex
import Reflex.Network
import Reflex.Class.Switchable
import Reflex.Vty


data Chopaan = Tx | Monitor
