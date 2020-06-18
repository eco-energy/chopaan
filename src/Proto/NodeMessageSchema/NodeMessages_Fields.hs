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
connectedChildren ::
                  forall f s a .
                    (Prelude.Functor f,
                     Data.ProtoLens.Field.HasField s "connectedChildren" a) =>
                    Lens.Family2.LensLike' f s a
connectedChildren = Data.ProtoLens.Field.field @"connectedChildren"
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
macAddress ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "macAddress" a) =>
             Lens.Family2.LensLike' f s a
macAddress = Data.ProtoLens.Field.field @"macAddress"
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
maybe'control ::
              forall f s a .
                (Prelude.Functor f,
                 Data.ProtoLens.Field.HasField s "maybe'control" a) =>
                Lens.Family2.LensLike' f s a
maybe'control = Data.ProtoLens.Field.field @"maybe'control"
maybe'hw ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "maybe'hw" a) =>
           Lens.Family2.LensLike' f s a
maybe'hw = Data.ProtoLens.Field.field @"maybe'hw"
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
minFreeHeap ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "minFreeHeap" a) =>
              Lens.Family2.LensLike' f s a
minFreeHeap = Data.ProtoLens.Field.field @"minFreeHeap"
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
uuid ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "uuid" a) =>
       Lens.Family2.LensLike' f s a
uuid = Data.ProtoLens.Field.field @"uuid"
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
wattSecondsTransacted ::
                      forall f s a .
                        (Prelude.Functor f,
                         Data.ProtoLens.Field.HasField s "wattSecondsTransacted" a) =>
                        Lens.Family2.LensLike' f s a
wattSecondsTransacted
  = Data.ProtoLens.Field.field @"wattSecondsTransacted"
wifiStrength ::
             forall f s a .
               (Prelude.Functor f,
                Data.ProtoLens.Field.HasField s "wifiStrength" a) =>
               Lens.Family2.LensLike' f s a
wifiStrength = Data.ProtoLens.Field.field @"wifiStrength"