{- This file was auto-generated from node_message_schema/NodeMessages.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds,
  BangPatterns, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.NodeMessageSchema.NodeMessages_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens
       as Data.ProtoLens
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes
       as Data.ProtoLens.Encoding.Bytes
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing
       as Data.ProtoLens.Encoding.Growing
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe
       as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire
       as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field
       as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum
       as Data.ProtoLens.Message.Enum
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types
       as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2
       as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked
       as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString
       as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8
       as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding
       as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic
       as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed
       as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read

ampHours ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "ampHours" a) =>
           Lens.Family2.LensLike' f s a
ampHours = Data.ProtoLens.Field.field @"ampHours"
battery ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "battery" a) =>
          Lens.Family2.LensLike' f s a
battery = Data.ProtoLens.Field.field @"battery"
batteryToGridCurrent ::
                     forall f s a .
                       (Prelude.Functor f,
                        Data.ProtoLens.Field.HasField s "batteryToGridCurrent" a) =>
                       Lens.Family2.LensLike' f s a
batteryToGridCurrent
  = Data.ProtoLens.Field.field @"batteryToGridCurrent"
batteryToLoadCurrent ::
                     forall f s a .
                       (Prelude.Functor f,
                        Data.ProtoLens.Field.HasField s "batteryToLoadCurrent" a) =>
                       Lens.Family2.LensLike' f s a
batteryToLoadCurrent
  = Data.ProtoLens.Field.field @"batteryToLoadCurrent"
batteryVoltage ::
               forall f s a .
                 (Prelude.Functor f,
                  Data.ProtoLens.Field.HasField s "batteryVoltage" a) =>
                 Lens.Family2.LensLike' f s a
batteryVoltage = Data.ProtoLens.Field.field @"batteryVoltage"
child ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "child" a) =>
        Lens.Family2.LensLike' f s a
child = Data.ProtoLens.Field.field @"child"
control ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "control" a) =>
          Lens.Family2.LensLike' f s a
control = Data.ProtoLens.Field.field @"control"
cpuTime ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "cpuTime" a) =>
          Lens.Family2.LensLike' f s a
cpuTime = Data.ProtoLens.Field.field @"cpuTime"
cpuUtilization ::
               forall f s a .
                 (Prelude.Functor f,
                  Data.ProtoLens.Field.HasField s "cpuUtilization" a) =>
                 Lens.Family2.LensLike' f s a
cpuUtilization = Data.ProtoLens.Field.field @"cpuUtilization"
currentFreeHeap ::
                forall f s a .
                  (Prelude.Functor f,
                   Data.ProtoLens.Field.HasField s "currentFreeHeap" a) =>
                  Lens.Family2.LensLike' f s a
currentFreeHeap = Data.ProtoLens.Field.field @"currentFreeHeap"
cutOffVoltage ::
              forall f s a .
                (Prelude.Functor f,
                 Data.ProtoLens.Field.HasField s "cutOffVoltage" a) =>
                Lens.Family2.LensLike' f s a
cutOffVoltage = Data.ProtoLens.Field.field @"cutOffVoltage"
direction ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "direction" a) =>
            Lens.Family2.LensLike' f s a
direction = Data.ProtoLens.Field.field @"direction"
disconnectGrid ::
               forall f s a .
                 (Prelude.Functor f,
                  Data.ProtoLens.Field.HasField s "disconnectGrid" a) =>
                 Lens.Family2.LensLike' f s a
disconnectGrid = Data.ProtoLens.Field.field @"disconnectGrid"
disconnectLoad ::
               forall f s a .
                 (Prelude.Functor f,
                  Data.ProtoLens.Field.HasField s "disconnectLoad" a) =>
                 Lens.Family2.LensLike' f s a
disconnectLoad = Data.ProtoLens.Field.field @"disconnectLoad"
disconnectSolar ::
                forall f s a .
                  (Prelude.Functor f,
                   Data.ProtoLens.Field.HasField s "disconnectSolar" a) =>
                  Lens.Family2.LensLike' f s a
disconnectSolar = Data.ProtoLens.Field.field @"disconnectSolar"
durationInSeconds ::
                  forall f s a .
                    (Prelude.Functor f,
                     Data.ProtoLens.Field.HasField s "durationInSeconds" a) =>
                    Lens.Family2.LensLike' f s a
durationInSeconds = Data.ProtoLens.Field.field @"durationInSeconds"
dutyCycle ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "dutyCycle" a) =>
            Lens.Family2.LensLike' f s a
dutyCycle = Data.ProtoLens.Field.field @"dutyCycle"
endpoint ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "endpoint" a) =>
           Lens.Family2.LensLike' f s a
endpoint = Data.ProtoLens.Field.field @"endpoint"
etrs ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "etrs" a) =>
       Lens.Family2.LensLike' f s a
etrs = Data.ProtoLens.Field.field @"etrs"
forceUpdate ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "forceUpdate" a) =>
              Lens.Family2.LensLike' f s a
forceUpdate = Data.ProtoLens.Field.field @"forceUpdate"
forcedActions ::
              forall f s a .
                (Prelude.Functor f,
                 Data.ProtoLens.Field.HasField s "forcedActions" a) =>
                Lens.Family2.LensLike' f s a
forcedActions = Data.ProtoLens.Field.field @"forcedActions"
gridCurrent ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "gridCurrent" a) =>
              Lens.Family2.LensLike' f s a
gridCurrent = Data.ProtoLens.Field.field @"gridCurrent"
gridToBatteryCurrent ::
                     forall f s a .
                       (Prelude.Functor f,
                        Data.ProtoLens.Field.HasField s "gridToBatteryCurrent" a) =>
                       Lens.Family2.LensLike' f s a
gridToBatteryCurrent
  = Data.ProtoLens.Field.field @"gridToBatteryCurrent"
gridVoltage ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "gridVoltage" a) =>
              Lens.Family2.LensLike' f s a
gridVoltage = Data.ProtoLens.Field.field @"gridVoltage"
hw ::
   forall f s a .
     (Prelude.Functor f, Data.ProtoLens.Field.HasField s "hw" a) =>
     Lens.Family2.LensLike' f s a
hw = Data.ProtoLens.Field.field @"hw"
iMPPT ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "iMPPT" a) =>
        Lens.Family2.LensLike' f s a
iMPPT = Data.ProtoLens.Field.field @"iMPPT"
isRoot ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "isRoot" a) =>
         Lens.Family2.LensLike' f s a
isRoot = Data.ProtoLens.Field.field @"isRoot"
key ::
    forall f s a .
      (Prelude.Functor f, Data.ProtoLens.Field.HasField s "key" a) =>
      Lens.Family2.LensLike' f s a
key = Data.ProtoLens.Field.field @"key"
macAddr ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "macAddr" a) =>
          Lens.Family2.LensLike' f s a
macAddr = Data.ProtoLens.Field.field @"macAddr"
maxChildNodesPerLayer ::
                      forall f s a .
                        (Prelude.Functor f,
                         Data.ProtoLens.Field.HasField s "maxChildNodesPerLayer" a) =>
                        Lens.Family2.LensLike' f s a
maxChildNodesPerLayer
  = Data.ProtoLens.Field.field @"maxChildNodesPerLayer"
maxV ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "maxV" a) =>
       Lens.Family2.LensLike' f s a
maxV = Data.ProtoLens.Field.field @"maxV"
maybe'battery ::
              forall f s a .
                (Prelude.Functor f,
                 Data.ProtoLens.Field.HasField s "maybe'battery" a) =>
                Lens.Family2.LensLike' f s a
maybe'battery = Data.ProtoLens.Field.field @"maybe'battery"
maybe'child ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "maybe'child" a) =>
              Lens.Family2.LensLike' f s a
maybe'child = Data.ProtoLens.Field.field @"maybe'child"
maybe'control ::
              forall f s a .
                (Prelude.Functor f,
                 Data.ProtoLens.Field.HasField s "maybe'control" a) =>
                Lens.Family2.LensLike' f s a
maybe'control = Data.ProtoLens.Field.field @"maybe'control"
maybe'forcedActions ::
                    forall f s a .
                      (Prelude.Functor f,
                       Data.ProtoLens.Field.HasField s "maybe'forcedActions" a) =>
                      Lens.Family2.LensLike' f s a
maybe'forcedActions
  = Data.ProtoLens.Field.field @"maybe'forcedActions"
maybe'hw ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "maybe'hw" a) =>
           Lens.Family2.LensLike' f s a
maybe'hw = Data.ProtoLens.Field.field @"maybe'hw"
maybe'meshConf ::
               forall f s a .
                 (Prelude.Functor f,
                  Data.ProtoLens.Field.HasField s "maybe'meshConf" a) =>
                 Lens.Family2.LensLike' f s a
maybe'meshConf = Data.ProtoLens.Field.field @"maybe'meshConf"
maybe'meshversion ::
                  forall f s a .
                    (Prelude.Functor f,
                     Data.ProtoLens.Field.HasField s "maybe'meshversion" a) =>
                    Lens.Family2.LensLike' f s a
maybe'meshversion = Data.ProtoLens.Field.field @"maybe'meshversion"
maybe'nodeTxRequest ::
                    forall f s a .
                      (Prelude.Functor f,
                       Data.ProtoLens.Field.HasField s "maybe'nodeTxRequest" a) =>
                      Lens.Family2.LensLike' f s a
maybe'nodeTxRequest
  = Data.ProtoLens.Field.field @"maybe'nodeTxRequest"
maybe'otaConf ::
              forall f s a .
                (Prelude.Functor f,
                 Data.ProtoLens.Field.HasField s "maybe'otaConf" a) =>
                Lens.Family2.LensLike' f s a
maybe'otaConf = Data.ProtoLens.Field.field @"maybe'otaConf"
maybe'otastatus ::
                forall f s a .
                  (Prelude.Functor f,
                   Data.ProtoLens.Field.HasField s "maybe'otastatus" a) =>
                  Lens.Family2.LensLike' f s a
maybe'otastatus = Data.ProtoLens.Field.field @"maybe'otastatus"
maybe'parent ::
             forall f s a .
               (Prelude.Functor f,
                Data.ProtoLens.Field.HasField s "maybe'parent" a) =>
               Lens.Family2.LensLike' f s a
maybe'parent = Data.ProtoLens.Field.field @"maybe'parent"
maybe'payload ::
              forall f s a .
                (Prelude.Functor f,
                 Data.ProtoLens.Field.HasField s "maybe'payload" a) =>
                Lens.Family2.LensLike' f s a
maybe'payload = Data.ProtoLens.Field.field @"maybe'payload"
maybe'rtStats ::
              forall f s a .
                (Prelude.Functor f,
                 Data.ProtoLens.Field.HasField s "maybe'rtStats" a) =>
                Lens.Family2.LensLike' f s a
maybe'rtStats = Data.ProtoLens.Field.field @"maybe'rtStats"
maybe'solar ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "maybe'solar" a) =>
              Lens.Family2.LensLike' f s a
maybe'solar = Data.ProtoLens.Field.field @"maybe'solar"
maybe'state ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "maybe'state" a) =>
              Lens.Family2.LensLike' f s a
maybe'state = Data.ProtoLens.Field.field @"maybe'state"
maybe'transaction ::
                  forall f s a .
                    (Prelude.Functor f,
                     Data.ProtoLens.Field.HasField s "maybe'transaction" a) =>
                    Lens.Family2.LensLike' f s a
maybe'transaction = Data.ProtoLens.Field.field @"maybe'transaction"
maybe'transactionStatus ::
                        forall f s a .
                          (Prelude.Functor f,
                           Data.ProtoLens.Field.HasField s "maybe'transactionStatus" a) =>
                          Lens.Family2.LensLike' f s a
maybe'transactionStatus
  = Data.ProtoLens.Field.field @"maybe'transactionStatus"
maybe'value ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "maybe'value" a) =>
              Lens.Family2.LensLike' f s a
maybe'value = Data.ProtoLens.Field.field @"maybe'value"
meshConf ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "meshConf" a) =>
           Lens.Family2.LensLike' f s a
meshConf = Data.ProtoLens.Field.field @"meshConf"
meshName ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "meshName" a) =>
           Lens.Family2.LensLike' f s a
meshName = Data.ProtoLens.Field.field @"meshName"
meshParentStrength ::
                   forall f s a .
                     (Prelude.Functor f,
                      Data.ProtoLens.Field.HasField s "meshParentStrength" a) =>
                     Lens.Family2.LensLike' f s a
meshParentStrength
  = Data.ProtoLens.Field.field @"meshParentStrength"
meshPwd ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "meshPwd" a) =>
          Lens.Family2.LensLike' f s a
meshPwd = Data.ProtoLens.Field.field @"meshPwd"
meshversion ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "meshversion" a) =>
              Lens.Family2.LensLike' f s a
meshversion = Data.ProtoLens.Field.field @"meshversion"
minFreeHeap ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "minFreeHeap" a) =>
              Lens.Family2.LensLike' f s a
minFreeHeap = Data.ProtoLens.Field.field @"minFreeHeap"
mqttBroker ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "mqttBroker" a) =>
             Lens.Family2.LensLike' f s a
mqttBroker = Data.ProtoLens.Field.field @"mqttBroker"
needsRecon ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "needsRecon" a) =>
             Lens.Family2.LensLike' f s a
needsRecon = Data.ProtoLens.Field.field @"needsRecon"
nodeMac ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "nodeMac" a) =>
          Lens.Family2.LensLike' f s a
nodeMac = Data.ProtoLens.Field.field @"nodeMac"
nodeTxRequest ::
              forall f s a .
                (Prelude.Functor f,
                 Data.ProtoLens.Field.HasField s "nodeTxRequest" a) =>
                Lens.Family2.LensLike' f s a
nodeTxRequest = Data.ProtoLens.Field.field @"nodeTxRequest"
otaConf ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "otaConf" a) =>
          Lens.Family2.LensLike' f s a
otaConf = Data.ProtoLens.Field.field @"otaConf"
otastatus ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "otastatus" a) =>
            Lens.Family2.LensLike' f s a
otastatus = Data.ProtoLens.Field.field @"otastatus"
parent ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "parent" a) =>
         Lens.Family2.LensLike' f s a
parent = Data.ProtoLens.Field.field @"parent"
parentJoiningRssi ::
                  forall f s a .
                    (Prelude.Functor f,
                     Data.ProtoLens.Field.HasField s "parentJoiningRssi" a) =>
                    Lens.Family2.LensLike' f s a
parentJoiningRssi = Data.ProtoLens.Field.field @"parentJoiningRssi"
parentRssiThreshold ::
                    forall f s a .
                      (Prelude.Functor f,
                       Data.ProtoLens.Field.HasField s "parentRssiThreshold" a) =>
                      Lens.Family2.LensLike' f s a
parentRssiThreshold
  = Data.ProtoLens.Field.field @"parentRssiThreshold"
parentversion ::
              forall f s a .
                (Prelude.Functor f,
                 Data.ProtoLens.Field.HasField s "parentversion" a) =>
                Lens.Family2.LensLike' f s a
parentversion = Data.ProtoLens.Field.field @"parentversion"
powerInWatts ::
             forall f s a .
               (Prelude.Functor f,
                Data.ProtoLens.Field.HasField s "powerInWatts" a) =>
               Lens.Family2.LensLike' f s a
powerInWatts = Data.ProtoLens.Field.field @"powerInWatts"
ratedPower ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "ratedPower" a) =>
             Lens.Family2.LensLike' f s a
ratedPower = Data.ProtoLens.Field.field @"ratedPower"
restartEsp ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "restartEsp" a) =>
             Lens.Family2.LensLike' f s a
restartEsp = Data.ProtoLens.Field.field @"restartEsp"
rtStats ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "rtStats" a) =>
          Lens.Family2.LensLike' f s a
rtStats = Data.ProtoLens.Field.field @"rtStats"
secondsToCompletion ::
                    forall f s a .
                      (Prelude.Functor f,
                       Data.ProtoLens.Field.HasField s "secondsToCompletion" a) =>
                      Lens.Family2.LensLike' f s a
secondsToCompletion
  = Data.ProtoLens.Field.field @"secondsToCompletion"
solar ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "solar" a) =>
        Lens.Family2.LensLike' f s a
solar = Data.ProtoLens.Field.field @"solar"
solarInputCurrent ::
                  forall f s a .
                    (Prelude.Functor f,
                     Data.ProtoLens.Field.HasField s "solarInputCurrent" a) =>
                    Lens.Family2.LensLike' f s a
solarInputCurrent = Data.ProtoLens.Field.field @"solarInputCurrent"
solarVoltage ::
             forall f s a .
               (Prelude.Functor f,
                Data.ProtoLens.Field.HasField s "solarVoltage" a) =>
               Lens.Family2.LensLike' f s a
solarVoltage = Data.ProtoLens.Field.field @"solarVoltage"
start ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "start" a) =>
        Lens.Family2.LensLike' f s a
start = Data.ProtoLens.Field.field @"start"
state ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "state" a) =>
        Lens.Family2.LensLike' f s a
state = Data.ProtoLens.Field.field @"state"
status ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "status" a) =>
         Lens.Family2.LensLike' f s a
status = Data.ProtoLens.Field.field @"status"
temperature ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "temperature" a) =>
              Lens.Family2.LensLike' f s a
temperature = Data.ProtoLens.Field.field @"temperature"
time ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "time" a) =>
       Lens.Family2.LensLike' f s a
time = Data.ProtoLens.Field.field @"time"
timeOfDay ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "timeOfDay" a) =>
            Lens.Family2.LensLike' f s a
timeOfDay = Data.ProtoLens.Field.field @"timeOfDay"
timeOfLastUpdate ::
                 forall f s a .
                   (Prelude.Functor f,
                    Data.ProtoLens.Field.HasField s "timeOfLastUpdate" a) =>
                   Lens.Family2.LensLike' f s a
timeOfLastUpdate = Data.ProtoLens.Field.field @"timeOfLastUpdate"
transaction ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "transaction" a) =>
              Lens.Family2.LensLike' f s a
transaction = Data.ProtoLens.Field.field @"transaction"
transactionStatus ::
                  forall f s a .
                    (Prelude.Functor f,
                     Data.ProtoLens.Field.HasField s "transactionStatus" a) =>
                    Lens.Family2.LensLike' f s a
transactionStatus = Data.ProtoLens.Field.field @"transactionStatus"
type' ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "type'" a) =>
        Lens.Family2.LensLike' f s a
type' = Data.ProtoLens.Field.field @"type'"
updateStatus ::
             forall f s a .
               (Prelude.Functor f,
                Data.ProtoLens.Field.HasField s "updateStatus" a) =>
               Lens.Family2.LensLike' f s a
updateStatus = Data.ProtoLens.Field.field @"updateStatus"
uptime ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "uptime" a) =>
         Lens.Family2.LensLike' f s a
uptime = Data.ProtoLens.Field.field @"uptime"
vMPPT ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vMPPT" a) =>
        Lens.Family2.LensLike' f s a
vMPPT = Data.ProtoLens.Field.field @"vMPPT"
vOC ::
    forall f s a .
      (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vOC" a) =>
      Lens.Family2.LensLike' f s a
vOC = Data.ProtoLens.Field.field @"vOC"
value ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "value" a) =>
        Lens.Family2.LensLike' f s a
value = Data.ProtoLens.Field.field @"value"
version ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "version" a) =>
          Lens.Family2.LensLike' f s a
version = Data.ProtoLens.Field.field @"version"
wattSecondsTransacted ::
                      forall f s a .
                        (Prelude.Functor f,
                         Data.ProtoLens.Field.HasField s "wattSecondsTransacted" a) =>
                        Lens.Family2.LensLike' f s a
wattSecondsTransacted
  = Data.ProtoLens.Field.field @"wattSecondsTransacted"
wifiPwd ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "wifiPwd" a) =>
          Lens.Family2.LensLike' f s a
wifiPwd = Data.ProtoLens.Field.field @"wifiPwd"
wifiStrength ::
             forall f s a .
               (Prelude.Functor f,
                Data.ProtoLens.Field.HasField s "wifiStrength" a) =>
               Lens.Family2.LensLike' f s a
wifiStrength = Data.ProtoLens.Field.field @"wifiStrength"
wifiUname ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "wifiUname" a) =>
            Lens.Family2.LensLike' f s a
wifiUname = Data.ProtoLens.Field.field @"wifiUname"