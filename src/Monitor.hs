module Monitor where


import Network.MQTT.Client
import Network.MQTT.Types
import Data.Aeson


-- show which nodes are currently connected.
-- show which nodes that should be connected aren't, and the time they disconnected
-- confirm that all nodes are subscribed to their control topic

