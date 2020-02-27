module UI.Types where

data KibbutzUI = HHListUI | MonitorUI | TxListUI | TxFormUI TXFormField deriving (Eq, Ord, Show)

type TxNodeId = Int

data TXFormField = NodeField TxNodeId | ParticipatingField TxNodeId | PowerField TxNodeId | DurationField TxNodeId  deriving (Eq, Ord, Show)
