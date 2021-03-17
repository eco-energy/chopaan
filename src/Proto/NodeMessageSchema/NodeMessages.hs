{- This file was auto-generated from node_message_schema/NodeMessages.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds,
  BangPatterns, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.NodeMessageSchema.NodeMessages
       (Actions(), BatteryParameters(), BatteryParameters'BatteryType(..),
        BatteryParameters'BatteryType(),
        BatteryParameters'BatteryType'UnrecognizedValue, EnergyState(),
        EnergyTransactionRequest(), EnergyTransactionStatus(),
        HardwareConfig(), MeshConfig(), MeshFrame(), MeshFrame'Payload(..),
        _MeshFrame'Control, _MeshFrame'State, _MeshFrame'Transaction,
        _MeshFrame'TransactionStatus, _MeshFrame'Hw, _MeshFrame'RtStats,
        _MeshFrame'MeshConf, _MeshFrame'OtaConf, _MeshFrame'Meshversion,
        _MeshFrame'Parent, _MeshFrame'Child, _MeshFrame'ForcedActions,
        _MeshFrame'Otastatus, _MeshFrame'NodeTxRequest, NodeControl(),
        NodeId(), OTAConfig(), PDirection(..), PDirection(),
        PDirection'UnrecognizedValue, PVParameters(),
        ReconciliationChild(), ReconciliationParent(), RuntimeStats(),
        SetVersion(), StreamState(..), StreamState(),
        StreamState'UnrecognizedValue, Transaction(),
        Transaction'EtrsEntry(), UpdateStatus())
       where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq
       as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism
       as Data.ProtoLens.Prism
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

{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.restartEsp' @:: Lens' Actions Prelude.Bool@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.forceUpdate' @:: Lens' Actions Prelude.Bool@
 -}
data Actions = Actions{_Actions'restartEsp :: !Prelude.Bool,
                       _Actions'forceUpdate :: !Prelude.Bool,
                       _Actions'_unknownFields :: !Data.ProtoLens.FieldSet}
                 deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Actions where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Actions "restartEsp"
           (Prelude.Bool)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Actions'restartEsp
               (\ x__ y__ -> x__{_Actions'restartEsp = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Actions "forceUpdate"
           (Prelude.Bool)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Actions'forceUpdate
               (\ x__ y__ -> x__{_Actions'forceUpdate = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message Actions where
        messageName _ = Data.Text.pack "Actions"
        fieldsByTag
          = let restartEsp__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "restart_esp"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"restartEsp"))
                      :: Data.ProtoLens.FieldDescriptor Actions
                forceUpdate__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "force_update"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"forceUpdate"))
                      :: Data.ProtoLens.FieldDescriptor Actions
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, restartEsp__field_descriptor),
                 (Data.ProtoLens.Tag 2, forceUpdate__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Actions'_unknownFields
              (\ x__ y__ -> x__{_Actions'_unknownFields = y__})
        defMessage
          = Actions{_Actions'restartEsp = Data.ProtoLens.fieldDefault,
                    _Actions'forceUpdate = Data.ProtoLens.fieldDefault,
                    _Actions'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     Actions -> Data.ProtoLens.Encoding.Bytes.Parser Actions
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                8 -> do y <- (Prelude.fmap ((Prelude./=) 0)
                                                Data.ProtoLens.Encoding.Bytes.getVarInt)
                                               Data.ProtoLens.Encoding.Bytes.<?> "restart_esp"
                                        loop
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"restartEsp")
                                             y
                                             x)
                                16 -> do y <- (Prelude.fmap ((Prelude./=) 0)
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "force_update"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"forceUpdate")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "Actions"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"restartEsp") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 8) Data.Monoid.<>
                      ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                         (\ b -> if b then 1 else 0))
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"forceUpdate") _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 16) Data.Monoid.<>
                        ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                           (\ b -> if b then 1 else 0))
                          _v)
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Actions where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_Actions'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_Actions'restartEsp x__)
                    (Control.DeepSeq.deepseq (_Actions'forceUpdate x__) (()))))
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.cutOffVoltage' @:: Lens' BatteryParameters Prelude.Float@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maxV' @:: Lens' BatteryParameters Prelude.Float@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.ampHours' @:: Lens' BatteryParameters Prelude.Float@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.type'' @:: Lens' BatteryParameters BatteryParameters'BatteryType@
 -}
data BatteryParameters = BatteryParameters{_BatteryParameters'cutOffVoltage
                                           :: !Prelude.Float,
                                           _BatteryParameters'maxV :: !Prelude.Float,
                                           _BatteryParameters'ampHours :: !Prelude.Float,
                                           _BatteryParameters'type' ::
                                           !BatteryParameters'BatteryType,
                                           _BatteryParameters'_unknownFields ::
                                           !Data.ProtoLens.FieldSet}
                           deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show BatteryParameters where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField BatteryParameters
           "cutOffVoltage"
           (Prelude.Float)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _BatteryParameters'cutOffVoltage
               (\ x__ y__ -> x__{_BatteryParameters'cutOffVoltage = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField BatteryParameters "maxV"
           (Prelude.Float)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _BatteryParameters'maxV
               (\ x__ y__ -> x__{_BatteryParameters'maxV = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField BatteryParameters "ampHours"
           (Prelude.Float)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _BatteryParameters'ampHours
               (\ x__ y__ -> x__{_BatteryParameters'ampHours = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField BatteryParameters "type'"
           (BatteryParameters'BatteryType)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _BatteryParameters'type'
               (\ x__ y__ -> x__{_BatteryParameters'type' = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message BatteryParameters where
        messageName _ = Data.Text.pack "BatteryParameters"
        fieldsByTag
          = let cutOffVoltage__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "cutOffVoltage"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"cutOffVoltage"))
                      :: Data.ProtoLens.FieldDescriptor BatteryParameters
                maxV__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "maxV"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"maxV"))
                      :: Data.ProtoLens.FieldDescriptor BatteryParameters
                ampHours__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "ampHours"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"ampHours"))
                      :: Data.ProtoLens.FieldDescriptor BatteryParameters
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor BatteryParameters'BatteryType)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"type'"))
                      :: Data.ProtoLens.FieldDescriptor BatteryParameters
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 3, cutOffVoltage__field_descriptor),
                 (Data.ProtoLens.Tag 4, maxV__field_descriptor),
                 (Data.ProtoLens.Tag 5, ampHours__field_descriptor),
                 (Data.ProtoLens.Tag 6, type'__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _BatteryParameters'_unknownFields
              (\ x__ y__ -> x__{_BatteryParameters'_unknownFields = y__})
        defMessage
          = BatteryParameters{_BatteryParameters'cutOffVoltage =
                                Data.ProtoLens.fieldDefault,
                              _BatteryParameters'maxV = Data.ProtoLens.fieldDefault,
                              _BatteryParameters'ampHours = Data.ProtoLens.fieldDefault,
                              _BatteryParameters'type' = Data.ProtoLens.fieldDefault,
                              _BatteryParameters'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     BatteryParameters ->
                       Data.ProtoLens.Encoding.Bytes.Parser BatteryParameters
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                29 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToFloat
                                                 Data.ProtoLens.Encoding.Bytes.getFixed32)
                                                Data.ProtoLens.Encoding.Bytes.<?> "cutOffVoltage"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"cutOffVoltage")
                                              y
                                              x)
                                37 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToFloat
                                                 Data.ProtoLens.Encoding.Bytes.getFixed32)
                                                Data.ProtoLens.Encoding.Bytes.<?> "maxV"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"maxV") y
                                              x)
                                45 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToFloat
                                                 Data.ProtoLens.Encoding.Bytes.getFixed32)
                                                Data.ProtoLens.Encoding.Bytes.<?> "ampHours"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"ampHours")
                                              y
                                              x)
                                48 -> do y <- (Prelude.fmap Prelude.toEnum
                                                 (Prelude.fmap Prelude.fromIntegral
                                                    Data.ProtoLens.Encoding.Bytes.getVarInt))
                                                Data.ProtoLens.Encoding.Bytes.<?> "type"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"type'") y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "BatteryParameters"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"cutOffVoltage")
                          _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 29) Data.Monoid.<>
                      ((Data.ProtoLens.Encoding.Bytes.putFixed32) Prelude..
                         Data.ProtoLens.Encoding.Bytes.floatToWord)
                        _v)
                 Data.Monoid.<>
                 (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"maxV") _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 37) Data.Monoid.<>
                        ((Data.ProtoLens.Encoding.Bytes.putFixed32) Prelude..
                           Data.ProtoLens.Encoding.Bytes.floatToWord)
                          _v)
                   Data.Monoid.<>
                   (let _v
                          = Lens.Family2.view (Data.ProtoLens.Field.field @"ampHours") _x
                      in
                      if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                        Data.Monoid.mempty else
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 45) Data.Monoid.<>
                          ((Data.ProtoLens.Encoding.Bytes.putFixed32) Prelude..
                             Data.ProtoLens.Encoding.Bytes.floatToWord)
                            _v)
                     Data.Monoid.<>
                     (let _v
                            = Lens.Family2.view (Data.ProtoLens.Field.field @"type'") _x
                        in
                        if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty else
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 48) Data.Monoid.<>
                            (((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                                Prelude.fromIntegral)
                               Prelude.. Prelude.fromEnum)
                              _v)
                       Data.Monoid.<>
                       Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData BatteryParameters where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_BatteryParameters'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_BatteryParameters'cutOffVoltage x__)
                    (Control.DeepSeq.deepseq (_BatteryParameters'maxV x__)
                       (Control.DeepSeq.deepseq (_BatteryParameters'ampHours x__)
                          (Control.DeepSeq.deepseq (_BatteryParameters'type' x__) (()))))))
newtype BatteryParameters'BatteryType'UnrecognizedValue = BatteryParameters'BatteryType'UnrecognizedValue Data.Int.Int32
                                                            deriving (Prelude.Eq, Prelude.Ord,
                                                                      Prelude.Show)
data BatteryParameters'BatteryType = BatteryParameters'LAflooded
                                   | BatteryParameters'LAsealed
                                   | BatteryParameters'LithiumIon
                                   | BatteryParameters'BatteryType'Unrecognized !BatteryParameters'BatteryType'UnrecognizedValue
                                       deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum BatteryParameters'BatteryType
         where
        maybeToEnum 0 = Prelude.Just BatteryParameters'LAflooded
        maybeToEnum 1 = Prelude.Just BatteryParameters'LAsealed
        maybeToEnum 2 = Prelude.Just BatteryParameters'LithiumIon
        maybeToEnum k
          = Prelude.Just
              (BatteryParameters'BatteryType'Unrecognized
                 (BatteryParameters'BatteryType'UnrecognizedValue
                    (Prelude.fromIntegral k)))
        showEnum BatteryParameters'LAflooded = "LAflooded"
        showEnum BatteryParameters'LAsealed = "LAsealed"
        showEnum BatteryParameters'LithiumIon = "LithiumIon"
        showEnum
          (BatteryParameters'BatteryType'Unrecognized
             (BatteryParameters'BatteryType'UnrecognizedValue k))
          = Prelude.show k
        readEnum k
          | (k) Prelude.== "LAflooded" =
            Prelude.Just BatteryParameters'LAflooded
          | (k) Prelude.== "LAsealed" =
            Prelude.Just BatteryParameters'LAsealed
          | (k) Prelude.== "LithiumIon" =
            Prelude.Just BatteryParameters'LithiumIon
        readEnum k
          = (Text.Read.readMaybe k) Prelude.>>= Data.ProtoLens.maybeToEnum
instance Prelude.Bounded BatteryParameters'BatteryType where
        minBound = BatteryParameters'LAflooded
        maxBound = BatteryParameters'LithiumIon
instance Prelude.Enum BatteryParameters'BatteryType where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 (("toEnum: unknown value for enum BatteryType: ") Prelude.++
                    Prelude.show k__))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum BatteryParameters'LAflooded = 0
        fromEnum BatteryParameters'LAsealed = 1
        fromEnum BatteryParameters'LithiumIon = 2
        fromEnum
          (BatteryParameters'BatteryType'Unrecognized
             (BatteryParameters'BatteryType'UnrecognizedValue k))
          = Prelude.fromIntegral k
        succ BatteryParameters'LithiumIon
          = Prelude.error
              "BatteryParameters'BatteryType.succ: bad argument BatteryParameters'LithiumIon. This value would be out of bounds."
        succ BatteryParameters'LAflooded = BatteryParameters'LAsealed
        succ BatteryParameters'LAsealed = BatteryParameters'LithiumIon
        succ (BatteryParameters'BatteryType'Unrecognized _)
          = Prelude.error
              "BatteryParameters'BatteryType.succ: bad argument: unrecognized value"
        pred BatteryParameters'LAflooded
          = Prelude.error
              "BatteryParameters'BatteryType.pred: bad argument BatteryParameters'LAflooded. This value would be out of bounds."
        pred BatteryParameters'LAsealed = BatteryParameters'LAflooded
        pred BatteryParameters'LithiumIon = BatteryParameters'LAsealed
        pred (BatteryParameters'BatteryType'Unrecognized _)
          = Prelude.error
              "BatteryParameters'BatteryType.pred: bad argument: unrecognized value"
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault BatteryParameters'BatteryType
         where
        fieldDefault = BatteryParameters'LAflooded
instance Control.DeepSeq.NFData BatteryParameters'BatteryType where
        rnf x__ = Prelude.seq x__ (())
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.batteryVoltage' @:: Lens' EnergyState Prelude.Double@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.gridVoltage' @:: Lens' EnergyState Prelude.Double@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.batteryToLoadCurrent' @:: Lens' EnergyState Prelude.Double@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.batteryToGridCurrent' @:: Lens' EnergyState Prelude.Double@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.gridToBatteryCurrent' @:: Lens' EnergyState Prelude.Double@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.solarInputCurrent' @:: Lens' EnergyState Prelude.Double@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.temperature' @:: Lens' EnergyState Prelude.Double@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.dutyCycle' @:: Lens' EnergyState Prelude.Double@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.cpuTime' @:: Lens' EnergyState Data.Word.Word64@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.status' @:: Lens' EnergyState StreamState@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.gridCurrent' @:: Lens' EnergyState Prelude.Double@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.solarVoltage' @:: Lens' EnergyState Prelude.Double@
 -}
data EnergyState = EnergyState{_EnergyState'batteryVoltage ::
                               !Prelude.Double,
                               _EnergyState'gridVoltage :: !Prelude.Double,
                               _EnergyState'batteryToLoadCurrent :: !Prelude.Double,
                               _EnergyState'batteryToGridCurrent :: !Prelude.Double,
                               _EnergyState'gridToBatteryCurrent :: !Prelude.Double,
                               _EnergyState'solarInputCurrent :: !Prelude.Double,
                               _EnergyState'temperature :: !Prelude.Double,
                               _EnergyState'dutyCycle :: !Prelude.Double,
                               _EnergyState'cpuTime :: !Data.Word.Word64,
                               _EnergyState'status :: !StreamState,
                               _EnergyState'gridCurrent :: !Prelude.Double,
                               _EnergyState'solarVoltage :: !Prelude.Double,
                               _EnergyState'_unknownFields :: !Data.ProtoLens.FieldSet}
                     deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show EnergyState where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField EnergyState "batteryVoltage"
           (Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _EnergyState'batteryVoltage
               (\ x__ y__ -> x__{_EnergyState'batteryVoltage = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyState "gridVoltage"
           (Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _EnergyState'gridVoltage
               (\ x__ y__ -> x__{_EnergyState'gridVoltage = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyState
           "batteryToLoadCurrent"
           (Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _EnergyState'batteryToLoadCurrent
               (\ x__ y__ -> x__{_EnergyState'batteryToLoadCurrent = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyState
           "batteryToGridCurrent"
           (Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _EnergyState'batteryToGridCurrent
               (\ x__ y__ -> x__{_EnergyState'batteryToGridCurrent = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyState
           "gridToBatteryCurrent"
           (Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _EnergyState'gridToBatteryCurrent
               (\ x__ y__ -> x__{_EnergyState'gridToBatteryCurrent = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyState
           "solarInputCurrent"
           (Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _EnergyState'solarInputCurrent
               (\ x__ y__ -> x__{_EnergyState'solarInputCurrent = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyState "temperature"
           (Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _EnergyState'temperature
               (\ x__ y__ -> x__{_EnergyState'temperature = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyState "dutyCycle"
           (Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _EnergyState'dutyCycle
               (\ x__ y__ -> x__{_EnergyState'dutyCycle = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyState "cpuTime"
           (Data.Word.Word64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _EnergyState'cpuTime
               (\ x__ y__ -> x__{_EnergyState'cpuTime = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyState "status"
           (StreamState)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _EnergyState'status
               (\ x__ y__ -> x__{_EnergyState'status = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyState "gridCurrent"
           (Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _EnergyState'gridCurrent
               (\ x__ y__ -> x__{_EnergyState'gridCurrent = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyState "solarVoltage"
           (Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _EnergyState'solarVoltage
               (\ x__ y__ -> x__{_EnergyState'solarVoltage = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message EnergyState where
        messageName _ = Data.Text.pack "EnergyState"
        fieldsByTag
          = let batteryVoltage__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "batteryVoltage"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"batteryVoltage"))
                      :: Data.ProtoLens.FieldDescriptor EnergyState
                gridVoltage__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "gridVoltage"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"gridVoltage"))
                      :: Data.ProtoLens.FieldDescriptor EnergyState
                batteryToLoadCurrent__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "batteryToLoadCurrent"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"batteryToLoadCurrent"))
                      :: Data.ProtoLens.FieldDescriptor EnergyState
                batteryToGridCurrent__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "batteryToGridCurrent"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"batteryToGridCurrent"))
                      :: Data.ProtoLens.FieldDescriptor EnergyState
                gridToBatteryCurrent__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "gridToBatteryCurrent"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"gridToBatteryCurrent"))
                      :: Data.ProtoLens.FieldDescriptor EnergyState
                solarInputCurrent__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "solarInputCurrent"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"solarInputCurrent"))
                      :: Data.ProtoLens.FieldDescriptor EnergyState
                temperature__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "temperature"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"temperature"))
                      :: Data.ProtoLens.FieldDescriptor EnergyState
                dutyCycle__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "dutyCycle"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"dutyCycle"))
                      :: Data.ProtoLens.FieldDescriptor EnergyState
                cpuTime__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "cpu_time"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"cpuTime"))
                      :: Data.ProtoLens.FieldDescriptor EnergyState
                status__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "status"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor StreamState)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"status"))
                      :: Data.ProtoLens.FieldDescriptor EnergyState
                gridCurrent__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "gridCurrent"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"gridCurrent"))
                      :: Data.ProtoLens.FieldDescriptor EnergyState
                solarVoltage__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "solarVoltage"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"solarVoltage"))
                      :: Data.ProtoLens.FieldDescriptor EnergyState
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, batteryVoltage__field_descriptor),
                 (Data.ProtoLens.Tag 2, gridVoltage__field_descriptor),
                 (Data.ProtoLens.Tag 3, batteryToLoadCurrent__field_descriptor),
                 (Data.ProtoLens.Tag 4, batteryToGridCurrent__field_descriptor),
                 (Data.ProtoLens.Tag 5, gridToBatteryCurrent__field_descriptor),
                 (Data.ProtoLens.Tag 6, solarInputCurrent__field_descriptor),
                 (Data.ProtoLens.Tag 7, temperature__field_descriptor),
                 (Data.ProtoLens.Tag 8, dutyCycle__field_descriptor),
                 (Data.ProtoLens.Tag 9, cpuTime__field_descriptor),
                 (Data.ProtoLens.Tag 10, status__field_descriptor),
                 (Data.ProtoLens.Tag 11, gridCurrent__field_descriptor),
                 (Data.ProtoLens.Tag 12, solarVoltage__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _EnergyState'_unknownFields
              (\ x__ y__ -> x__{_EnergyState'_unknownFields = y__})
        defMessage
          = EnergyState{_EnergyState'batteryVoltage =
                          Data.ProtoLens.fieldDefault,
                        _EnergyState'gridVoltage = Data.ProtoLens.fieldDefault,
                        _EnergyState'batteryToLoadCurrent = Data.ProtoLens.fieldDefault,
                        _EnergyState'batteryToGridCurrent = Data.ProtoLens.fieldDefault,
                        _EnergyState'gridToBatteryCurrent = Data.ProtoLens.fieldDefault,
                        _EnergyState'solarInputCurrent = Data.ProtoLens.fieldDefault,
                        _EnergyState'temperature = Data.ProtoLens.fieldDefault,
                        _EnergyState'dutyCycle = Data.ProtoLens.fieldDefault,
                        _EnergyState'cpuTime = Data.ProtoLens.fieldDefault,
                        _EnergyState'status = Data.ProtoLens.fieldDefault,
                        _EnergyState'gridCurrent = Data.ProtoLens.fieldDefault,
                        _EnergyState'solarVoltage = Data.ProtoLens.fieldDefault,
                        _EnergyState'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     EnergyState -> Data.ProtoLens.Encoding.Bytes.Parser EnergyState
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                9 -> do y <- (Prelude.fmap
                                                Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                Data.ProtoLens.Encoding.Bytes.getFixed64)
                                               Data.ProtoLens.Encoding.Bytes.<?> "batteryVoltage"
                                        loop
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"batteryVoltage")
                                             y
                                             x)
                                17 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                 Data.ProtoLens.Encoding.Bytes.getFixed64)
                                                Data.ProtoLens.Encoding.Bytes.<?> "gridVoltage"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"gridVoltage")
                                              y
                                              x)
                                25 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                 Data.ProtoLens.Encoding.Bytes.getFixed64)
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "batteryToLoadCurrent"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"batteryToLoadCurrent")
                                              y
                                              x)
                                33 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                 Data.ProtoLens.Encoding.Bytes.getFixed64)
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "batteryToGridCurrent"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"batteryToGridCurrent")
                                              y
                                              x)
                                41 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                 Data.ProtoLens.Encoding.Bytes.getFixed64)
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "gridToBatteryCurrent"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"gridToBatteryCurrent")
                                              y
                                              x)
                                49 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                 Data.ProtoLens.Encoding.Bytes.getFixed64)
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "solarInputCurrent"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"solarInputCurrent")
                                              y
                                              x)
                                57 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                 Data.ProtoLens.Encoding.Bytes.getFixed64)
                                                Data.ProtoLens.Encoding.Bytes.<?> "temperature"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"temperature")
                                              y
                                              x)
                                65 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                 Data.ProtoLens.Encoding.Bytes.getFixed64)
                                                Data.ProtoLens.Encoding.Bytes.<?> "dutyCycle"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"dutyCycle")
                                              y
                                              x)
                                72 -> do y <- (Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "cpu_time"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"cpuTime")
                                              y
                                              x)
                                80 -> do y <- (Prelude.fmap Prelude.toEnum
                                                 (Prelude.fmap Prelude.fromIntegral
                                                    Data.ProtoLens.Encoding.Bytes.getVarInt))
                                                Data.ProtoLens.Encoding.Bytes.<?> "status"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"status")
                                              y
                                              x)
                                89 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                 Data.ProtoLens.Encoding.Bytes.getFixed64)
                                                Data.ProtoLens.Encoding.Bytes.<?> "gridCurrent"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"gridCurrent")
                                              y
                                              x)
                                97 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                 Data.ProtoLens.Encoding.Bytes.getFixed64)
                                                Data.ProtoLens.Encoding.Bytes.<?> "solarVoltage"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"solarVoltage")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "EnergyState"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"batteryVoltage")
                          _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 9) Data.Monoid.<>
                      ((Data.ProtoLens.Encoding.Bytes.putFixed64) Prelude..
                         Data.ProtoLens.Encoding.Bytes.doubleToWord)
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"gridVoltage") _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 17) Data.Monoid.<>
                        ((Data.ProtoLens.Encoding.Bytes.putFixed64) Prelude..
                           Data.ProtoLens.Encoding.Bytes.doubleToWord)
                          _v)
                   Data.Monoid.<>
                   (let _v
                          = Lens.Family2.view
                              (Data.ProtoLens.Field.field @"batteryToLoadCurrent")
                              _x
                      in
                      if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                        Data.Monoid.mempty else
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 25) Data.Monoid.<>
                          ((Data.ProtoLens.Encoding.Bytes.putFixed64) Prelude..
                             Data.ProtoLens.Encoding.Bytes.doubleToWord)
                            _v)
                     Data.Monoid.<>
                     (let _v
                            = Lens.Family2.view
                                (Data.ProtoLens.Field.field @"batteryToGridCurrent")
                                _x
                        in
                        if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty else
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 33) Data.Monoid.<>
                            ((Data.ProtoLens.Encoding.Bytes.putFixed64) Prelude..
                               Data.ProtoLens.Encoding.Bytes.doubleToWord)
                              _v)
                       Data.Monoid.<>
                       (let _v
                              = Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"gridToBatteryCurrent")
                                  _x
                          in
                          if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                            Data.Monoid.mempty else
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 41) Data.Monoid.<>
                              ((Data.ProtoLens.Encoding.Bytes.putFixed64) Prelude..
                                 Data.ProtoLens.Encoding.Bytes.doubleToWord)
                                _v)
                         Data.Monoid.<>
                         (let _v
                                = Lens.Family2.view
                                    (Data.ProtoLens.Field.field @"solarInputCurrent")
                                    _x
                            in
                            if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                              Data.Monoid.mempty else
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 49) Data.Monoid.<>
                                ((Data.ProtoLens.Encoding.Bytes.putFixed64) Prelude..
                                   Data.ProtoLens.Encoding.Bytes.doubleToWord)
                                  _v)
                           Data.Monoid.<>
                           (let _v
                                  = Lens.Family2.view (Data.ProtoLens.Field.field @"temperature") _x
                              in
                              if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty else
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 57) Data.Monoid.<>
                                  ((Data.ProtoLens.Encoding.Bytes.putFixed64) Prelude..
                                     Data.ProtoLens.Encoding.Bytes.doubleToWord)
                                    _v)
                             Data.Monoid.<>
                             (let _v
                                    = Lens.Family2.view (Data.ProtoLens.Field.field @"dutyCycle") _x
                                in
                                if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                                  Data.Monoid.mempty else
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 65) Data.Monoid.<>
                                    ((Data.ProtoLens.Encoding.Bytes.putFixed64) Prelude..
                                       Data.ProtoLens.Encoding.Bytes.doubleToWord)
                                      _v)
                               Data.Monoid.<>
                               (let _v
                                      = Lens.Family2.view (Data.ProtoLens.Field.field @"cpuTime") _x
                                  in
                                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                                    Data.Monoid.mempty else
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 72) Data.Monoid.<>
                                      Data.ProtoLens.Encoding.Bytes.putVarInt _v)
                                 Data.Monoid.<>
                                 (let _v
                                        = Lens.Family2.view (Data.ProtoLens.Field.field @"status")
                                            _x
                                    in
                                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                                      Data.Monoid.mempty else
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt 80) Data.Monoid.<>
                                        (((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                                            Prelude.fromIntegral)
                                           Prelude.. Prelude.fromEnum)
                                          _v)
                                   Data.Monoid.<>
                                   (let _v
                                          = Lens.Family2.view
                                              (Data.ProtoLens.Field.field @"gridCurrent")
                                              _x
                                      in
                                      if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                                        Data.Monoid.mempty else
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 89) Data.Monoid.<>
                                          ((Data.ProtoLens.Encoding.Bytes.putFixed64) Prelude..
                                             Data.ProtoLens.Encoding.Bytes.doubleToWord)
                                            _v)
                                     Data.Monoid.<>
                                     (let _v
                                            = Lens.Family2.view
                                                (Data.ProtoLens.Field.field @"solarVoltage")
                                                _x
                                        in
                                        if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                                          Data.Monoid.mempty else
                                          (Data.ProtoLens.Encoding.Bytes.putVarInt 97)
                                            Data.Monoid.<>
                                            ((Data.ProtoLens.Encoding.Bytes.putFixed64) Prelude..
                                               Data.ProtoLens.Encoding.Bytes.doubleToWord)
                                              _v)
                                       Data.Monoid.<>
                                       Data.ProtoLens.Encoding.Wire.buildFieldSet
                                         (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData EnergyState where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_EnergyState'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_EnergyState'batteryVoltage x__)
                    (Control.DeepSeq.deepseq (_EnergyState'gridVoltage x__)
                       (Control.DeepSeq.deepseq (_EnergyState'batteryToLoadCurrent x__)
                          (Control.DeepSeq.deepseq (_EnergyState'batteryToGridCurrent x__)
                             (Control.DeepSeq.deepseq (_EnergyState'gridToBatteryCurrent x__)
                                (Control.DeepSeq.deepseq (_EnergyState'solarInputCurrent x__)
                                   (Control.DeepSeq.deepseq (_EnergyState'temperature x__)
                                      (Control.DeepSeq.deepseq (_EnergyState'dutyCycle x__)
                                         (Control.DeepSeq.deepseq (_EnergyState'cpuTime x__)
                                            (Control.DeepSeq.deepseq (_EnergyState'status x__)
                                               (Control.DeepSeq.deepseq
                                                  (_EnergyState'gridCurrent x__)
                                                  (Control.DeepSeq.deepseq
                                                     (_EnergyState'solarVoltage x__)
                                                     (()))))))))))))))
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.powerInWatts' @:: Lens' EnergyTransactionRequest Prelude.Double@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.durationInSeconds' @:: Lens' EnergyTransactionRequest Data.Word.Word64@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.direction' @:: Lens' EnergyTransactionRequest PDirection@
 -}
data EnergyTransactionRequest = EnergyTransactionRequest{_EnergyTransactionRequest'powerInWatts
                                                         :: !Prelude.Double,
                                                         _EnergyTransactionRequest'durationInSeconds
                                                         :: !Data.Word.Word64,
                                                         _EnergyTransactionRequest'direction ::
                                                         !PDirection,
                                                         _EnergyTransactionRequest'_unknownFields ::
                                                         !Data.ProtoLens.FieldSet}
                                  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show EnergyTransactionRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField EnergyTransactionRequest
           "powerInWatts"
           (Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens
               _EnergyTransactionRequest'powerInWatts
               (\ x__ y__ -> x__{_EnergyTransactionRequest'powerInWatts = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyTransactionRequest
           "durationInSeconds"
           (Data.Word.Word64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens
               _EnergyTransactionRequest'durationInSeconds
               (\ x__ y__ ->
                  x__{_EnergyTransactionRequest'durationInSeconds = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyTransactionRequest
           "direction"
           (PDirection)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _EnergyTransactionRequest'direction
               (\ x__ y__ -> x__{_EnergyTransactionRequest'direction = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message EnergyTransactionRequest where
        messageName _ = Data.Text.pack "EnergyTransactionRequest"
        fieldsByTag
          = let powerInWatts__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "powerInWatts"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"powerInWatts"))
                      :: Data.ProtoLens.FieldDescriptor EnergyTransactionRequest
                durationInSeconds__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "durationInSeconds"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"durationInSeconds"))
                      :: Data.ProtoLens.FieldDescriptor EnergyTransactionRequest
                direction__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "direction"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor PDirection)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"direction"))
                      :: Data.ProtoLens.FieldDescriptor EnergyTransactionRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, powerInWatts__field_descriptor),
                 (Data.ProtoLens.Tag 2, durationInSeconds__field_descriptor),
                 (Data.ProtoLens.Tag 3, direction__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens
              _EnergyTransactionRequest'_unknownFields
              (\ x__ y__ -> x__{_EnergyTransactionRequest'_unknownFields = y__})
        defMessage
          = EnergyTransactionRequest{_EnergyTransactionRequest'powerInWatts =
                                       Data.ProtoLens.fieldDefault,
                                     _EnergyTransactionRequest'durationInSeconds =
                                       Data.ProtoLens.fieldDefault,
                                     _EnergyTransactionRequest'direction =
                                       Data.ProtoLens.fieldDefault,
                                     _EnergyTransactionRequest'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     EnergyTransactionRequest ->
                       Data.ProtoLens.Encoding.Bytes.Parser EnergyTransactionRequest
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                9 -> do y <- (Prelude.fmap
                                                Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                Data.ProtoLens.Encoding.Bytes.getFixed64)
                                               Data.ProtoLens.Encoding.Bytes.<?> "powerInWatts"
                                        loop
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"powerInWatts")
                                             y
                                             x)
                                16 -> do y <- (Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "durationInSeconds"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"durationInSeconds")
                                              y
                                              x)
                                24 -> do y <- (Prelude.fmap Prelude.toEnum
                                                 (Prelude.fmap Prelude.fromIntegral
                                                    Data.ProtoLens.Encoding.Bytes.getVarInt))
                                                Data.ProtoLens.Encoding.Bytes.<?> "direction"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"direction")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "EnergyTransactionRequest"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"powerInWatts") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 9) Data.Monoid.<>
                      ((Data.ProtoLens.Encoding.Bytes.putFixed64) Prelude..
                         Data.ProtoLens.Encoding.Bytes.doubleToWord)
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"durationInSeconds")
                            _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 16) Data.Monoid.<>
                        Data.ProtoLens.Encoding.Bytes.putVarInt _v)
                   Data.Monoid.<>
                   (let _v
                          = Lens.Family2.view (Data.ProtoLens.Field.field @"direction") _x
                      in
                      if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                        Data.Monoid.mempty else
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 24) Data.Monoid.<>
                          (((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                              Prelude.fromIntegral)
                             Prelude.. Prelude.fromEnum)
                            _v)
                     Data.Monoid.<>
                     Data.ProtoLens.Encoding.Wire.buildFieldSet
                       (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData EnergyTransactionRequest where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq
                 (_EnergyTransactionRequest'_unknownFields x__)
                 (Control.DeepSeq.deepseq
                    (_EnergyTransactionRequest'powerInWatts x__)
                    (Control.DeepSeq.deepseq
                       (_EnergyTransactionRequest'durationInSeconds x__)
                       (Control.DeepSeq.deepseq (_EnergyTransactionRequest'direction x__)
                          (())))))
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.uuid' @:: Lens' EnergyTransactionStatus Data.Text.Text@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.wattSecondsTransacted' @:: Lens' EnergyTransactionStatus Prelude.Double@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.secondsToCompletion' @:: Lens' EnergyTransactionStatus Data.Word.Word64@
 -}
data EnergyTransactionStatus = EnergyTransactionStatus{_EnergyTransactionStatus'uuid
                                                       :: !Data.Text.Text,
                                                       _EnergyTransactionStatus'wattSecondsTransacted
                                                       :: !Prelude.Double,
                                                       _EnergyTransactionStatus'secondsToCompletion
                                                       :: !Data.Word.Word64,
                                                       _EnergyTransactionStatus'_unknownFields ::
                                                       !Data.ProtoLens.FieldSet}
                                 deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show EnergyTransactionStatus where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField EnergyTransactionStatus
           "uuid"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _EnergyTransactionStatus'uuid
               (\ x__ y__ -> x__{_EnergyTransactionStatus'uuid = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyTransactionStatus
           "wattSecondsTransacted"
           (Prelude.Double)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens
               _EnergyTransactionStatus'wattSecondsTransacted
               (\ x__ y__ ->
                  x__{_EnergyTransactionStatus'wattSecondsTransacted = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField EnergyTransactionStatus
           "secondsToCompletion"
           (Data.Word.Word64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens
               _EnergyTransactionStatus'secondsToCompletion
               (\ x__ y__ ->
                  x__{_EnergyTransactionStatus'secondsToCompletion = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message EnergyTransactionStatus where
        messageName _ = Data.Text.pack "EnergyTransactionStatus"
        fieldsByTag
          = let uuid__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "uuid"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"uuid"))
                      :: Data.ProtoLens.FieldDescriptor EnergyTransactionStatus
                wattSecondsTransacted__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "wattSecondsTransacted"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"wattSecondsTransacted"))
                      :: Data.ProtoLens.FieldDescriptor EnergyTransactionStatus
                secondsToCompletion__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "secondsToCompletion"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"secondsToCompletion"))
                      :: Data.ProtoLens.FieldDescriptor EnergyTransactionStatus
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, uuid__field_descriptor),
                 (Data.ProtoLens.Tag 2, wattSecondsTransacted__field_descriptor),
                 (Data.ProtoLens.Tag 3, secondsToCompletion__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens
              _EnergyTransactionStatus'_unknownFields
              (\ x__ y__ -> x__{_EnergyTransactionStatus'_unknownFields = y__})
        defMessage
          = EnergyTransactionStatus{_EnergyTransactionStatus'uuid =
                                      Data.ProtoLens.fieldDefault,
                                    _EnergyTransactionStatus'wattSecondsTransacted =
                                      Data.ProtoLens.fieldDefault,
                                    _EnergyTransactionStatus'secondsToCompletion =
                                      Data.ProtoLens.fieldDefault,
                                    _EnergyTransactionStatus'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     EnergyTransactionStatus ->
                       Data.ProtoLens.Encoding.Bytes.Parser EnergyTransactionStatus
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "uuid"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"uuid") y
                                              x)
                                17 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToDouble
                                                 Data.ProtoLens.Encoding.Bytes.getFixed64)
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "wattSecondsTransacted"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"wattSecondsTransacted")
                                              y
                                              x)
                                24 -> do y <- (Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "secondsToCompletion"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"secondsToCompletion")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "EnergyTransactionStatus"
        buildMessage
          = (\ _x ->
               (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"uuid") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                      (((\ bs ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                             Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Prelude.. Data.Text.Encoding.encodeUtf8)
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"wattSecondsTransacted")
                            _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 17) Data.Monoid.<>
                        ((Data.ProtoLens.Encoding.Bytes.putFixed64) Prelude..
                           Data.ProtoLens.Encoding.Bytes.doubleToWord)
                          _v)
                   Data.Monoid.<>
                   (let _v
                          = Lens.Family2.view
                              (Data.ProtoLens.Field.field @"secondsToCompletion")
                              _x
                      in
                      if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                        Data.Monoid.mempty else
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 24) Data.Monoid.<>
                          Data.ProtoLens.Encoding.Bytes.putVarInt _v)
                     Data.Monoid.<>
                     Data.ProtoLens.Encoding.Wire.buildFieldSet
                       (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData EnergyTransactionStatus where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq
                 (_EnergyTransactionStatus'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_EnergyTransactionStatus'uuid x__)
                    (Control.DeepSeq.deepseq
                       (_EnergyTransactionStatus'wattSecondsTransacted x__)
                       (Control.DeepSeq.deepseq
                          (_EnergyTransactionStatus'secondsToCompletion x__)
                          (())))))
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.battery' @:: Lens' HardwareConfig BatteryParameters@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'battery' @:: Lens' HardwareConfig (Prelude.Maybe BatteryParameters)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.solar' @:: Lens' HardwareConfig PVParameters@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'solar' @:: Lens' HardwareConfig (Prelude.Maybe PVParameters)@
 -}
data HardwareConfig = HardwareConfig{_HardwareConfig'battery ::
                                     !(Prelude.Maybe BatteryParameters),
                                     _HardwareConfig'solar :: !(Prelude.Maybe PVParameters),
                                     _HardwareConfig'_unknownFields :: !Data.ProtoLens.FieldSet}
                        deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show HardwareConfig where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField HardwareConfig "battery"
           (BatteryParameters)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HardwareConfig'battery
               (\ x__ y__ -> x__{_HardwareConfig'battery = y__}))
              Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField HardwareConfig
           "maybe'battery"
           (Prelude.Maybe BatteryParameters)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HardwareConfig'battery
               (\ x__ y__ -> x__{_HardwareConfig'battery = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField HardwareConfig "solar"
           (PVParameters)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HardwareConfig'solar
               (\ x__ y__ -> x__{_HardwareConfig'solar = y__}))
              Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField HardwareConfig "maybe'solar"
           (Prelude.Maybe PVParameters)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _HardwareConfig'solar
               (\ x__ y__ -> x__{_HardwareConfig'solar = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message HardwareConfig where
        messageName _ = Data.Text.pack "HardwareConfig"
        fieldsByTag
          = let battery__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "battery"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor BatteryParameters)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'battery"))
                      :: Data.ProtoLens.FieldDescriptor HardwareConfig
                solar__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "solar"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor PVParameters)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'solar"))
                      :: Data.ProtoLens.FieldDescriptor HardwareConfig
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, battery__field_descriptor),
                 (Data.ProtoLens.Tag 2, solar__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _HardwareConfig'_unknownFields
              (\ x__ y__ -> x__{_HardwareConfig'_unknownFields = y__})
        defMessage
          = HardwareConfig{_HardwareConfig'battery = Prelude.Nothing,
                           _HardwareConfig'solar = Prelude.Nothing,
                           _HardwareConfig'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     HardwareConfig ->
                       Data.ProtoLens.Encoding.Bytes.Parser HardwareConfig
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "battery"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"battery")
                                              y
                                              x)
                                18 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "solar"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"solar") y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "HardwareConfig"
        buildMessage
          = (\ _x ->
               (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'battery") _x
                  of
                    (Prelude.Nothing) -> Data.Monoid.mempty
                    Prelude.Just _v -> (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                                         Data.Monoid.<>
                                         (((\ bs ->
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                                Data.Monoid.<>
                                                Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                            Prelude.. Data.ProtoLens.encodeMessage)
                                           _v)
                 Data.Monoid.<>
                 (case
                    Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'solar") _x of
                      (Prelude.Nothing) -> Data.Monoid.mempty
                      Prelude.Just _v -> (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                                           Data.Monoid.<>
                                           (((\ bs ->
                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                   (Prelude.fromIntegral
                                                      (Data.ByteString.length bs)))
                                                  Data.Monoid.<>
                                                  Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                              Prelude.. Data.ProtoLens.encodeMessage)
                                             _v)
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData HardwareConfig where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_HardwareConfig'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_HardwareConfig'battery x__)
                    (Control.DeepSeq.deepseq (_HardwareConfig'solar x__) (()))))
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.wifiUname' @:: Lens' MeshConfig Data.Text.Text@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.wifiPwd' @:: Lens' MeshConfig Data.Text.Text@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.meshName' @:: Lens' MeshConfig Data.Text.Text@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.meshPwd' @:: Lens' MeshConfig Data.Text.Text@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.mqttBroker' @:: Lens' MeshConfig Data.Text.Text@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.parentRssiThreshold' @:: Lens' MeshConfig Data.Word.Word32@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.parenJoiningRssi' @:: Lens' MeshConfig Data.Word.Word32@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maxChildNodesPerLayer' @:: Lens' MeshConfig Data.Word.Word32@
 -}
data MeshConfig = MeshConfig{_MeshConfig'wifiUname ::
                             !Data.Text.Text,
                             _MeshConfig'wifiPwd :: !Data.Text.Text,
                             _MeshConfig'meshName :: !Data.Text.Text,
                             _MeshConfig'meshPwd :: !Data.Text.Text,
                             _MeshConfig'mqttBroker :: !Data.Text.Text,
                             _MeshConfig'parentRssiThreshold :: !Data.Word.Word32,
                             _MeshConfig'parenJoiningRssi :: !Data.Word.Word32,
                             _MeshConfig'maxChildNodesPerLayer :: !Data.Word.Word32,
                             _MeshConfig'_unknownFields :: !Data.ProtoLens.FieldSet}
                    deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show MeshConfig where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField MeshConfig "wifiUname"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshConfig'wifiUname
               (\ x__ y__ -> x__{_MeshConfig'wifiUname = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField MeshConfig "wifiPwd"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshConfig'wifiPwd
               (\ x__ y__ -> x__{_MeshConfig'wifiPwd = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField MeshConfig "meshName"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshConfig'meshName
               (\ x__ y__ -> x__{_MeshConfig'meshName = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField MeshConfig "meshPwd"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshConfig'meshPwd
               (\ x__ y__ -> x__{_MeshConfig'meshPwd = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField MeshConfig "mqttBroker"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshConfig'mqttBroker
               (\ x__ y__ -> x__{_MeshConfig'mqttBroker = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField MeshConfig
           "parentRssiThreshold"
           (Data.Word.Word32)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshConfig'parentRssiThreshold
               (\ x__ y__ -> x__{_MeshConfig'parentRssiThreshold = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField MeshConfig
           "parenJoiningRssi"
           (Data.Word.Word32)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshConfig'parenJoiningRssi
               (\ x__ y__ -> x__{_MeshConfig'parenJoiningRssi = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField MeshConfig
           "maxChildNodesPerLayer"
           (Data.Word.Word32)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshConfig'maxChildNodesPerLayer
               (\ x__ y__ -> x__{_MeshConfig'maxChildNodesPerLayer = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message MeshConfig where
        messageName _ = Data.Text.pack "MeshConfig"
        fieldsByTag
          = let wifiUname__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "wifiUname"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"wifiUname"))
                      :: Data.ProtoLens.FieldDescriptor MeshConfig
                wifiPwd__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "wifiPwd"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"wifiPwd"))
                      :: Data.ProtoLens.FieldDescriptor MeshConfig
                meshName__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "meshName"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"meshName"))
                      :: Data.ProtoLens.FieldDescriptor MeshConfig
                meshPwd__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "meshPwd"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"meshPwd"))
                      :: Data.ProtoLens.FieldDescriptor MeshConfig
                mqttBroker__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "mqttBroker"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"mqttBroker"))
                      :: Data.ProtoLens.FieldDescriptor MeshConfig
                parentRssiThreshold__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "ParentRssiThreshold"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"parentRssiThreshold"))
                      :: Data.ProtoLens.FieldDescriptor MeshConfig
                parenJoiningRssi__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "ParenJoiningRssi"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"parenJoiningRssi"))
                      :: Data.ProtoLens.FieldDescriptor MeshConfig
                maxChildNodesPerLayer__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "MaxChildNodesPerLayer"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"maxChildNodesPerLayer"))
                      :: Data.ProtoLens.FieldDescriptor MeshConfig
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, wifiUname__field_descriptor),
                 (Data.ProtoLens.Tag 2, wifiPwd__field_descriptor),
                 (Data.ProtoLens.Tag 3, meshName__field_descriptor),
                 (Data.ProtoLens.Tag 4, meshPwd__field_descriptor),
                 (Data.ProtoLens.Tag 5, mqttBroker__field_descriptor),
                 (Data.ProtoLens.Tag 6, parentRssiThreshold__field_descriptor),
                 (Data.ProtoLens.Tag 7, parenJoiningRssi__field_descriptor),
                 (Data.ProtoLens.Tag 8, maxChildNodesPerLayer__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _MeshConfig'_unknownFields
              (\ x__ y__ -> x__{_MeshConfig'_unknownFields = y__})
        defMessage
          = MeshConfig{_MeshConfig'wifiUname = Data.ProtoLens.fieldDefault,
                       _MeshConfig'wifiPwd = Data.ProtoLens.fieldDefault,
                       _MeshConfig'meshName = Data.ProtoLens.fieldDefault,
                       _MeshConfig'meshPwd = Data.ProtoLens.fieldDefault,
                       _MeshConfig'mqttBroker = Data.ProtoLens.fieldDefault,
                       _MeshConfig'parentRssiThreshold = Data.ProtoLens.fieldDefault,
                       _MeshConfig'parenJoiningRssi = Data.ProtoLens.fieldDefault,
                       _MeshConfig'maxChildNodesPerLayer = Data.ProtoLens.fieldDefault,
                       _MeshConfig'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     MeshConfig -> Data.ProtoLens.Encoding.Bytes.Parser MeshConfig
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "wifiUname"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"wifiUname")
                                              y
                                              x)
                                18 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "wifiPwd"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"wifiPwd")
                                              y
                                              x)
                                26 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "meshName"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"meshName")
                                              y
                                              x)
                                34 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "meshPwd"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"meshPwd")
                                              y
                                              x)
                                42 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "mqttBroker"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"mqttBroker")
                                              y
                                              x)
                                48 -> do y <- (Prelude.fmap Prelude.fromIntegral
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "ParentRssiThreshold"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"parentRssiThreshold")
                                              y
                                              x)
                                56 -> do y <- (Prelude.fmap Prelude.fromIntegral
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "ParenJoiningRssi"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"parenJoiningRssi")
                                              y
                                              x)
                                64 -> do y <- (Prelude.fmap Prelude.fromIntegral
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "MaxChildNodesPerLayer"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"maxChildNodesPerLayer")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "MeshConfig"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"wifiUname") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                      (((\ bs ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                             Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Prelude.. Data.Text.Encoding.encodeUtf8)
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"wifiPwd") _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 18) Data.Monoid.<>
                        (((\ bs ->
                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                (Prelude.fromIntegral (Data.ByteString.length bs)))
                               Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Prelude.. Data.Text.Encoding.encodeUtf8)
                          _v)
                   Data.Monoid.<>
                   (let _v
                          = Lens.Family2.view (Data.ProtoLens.Field.field @"meshName") _x
                      in
                      if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                        Data.Monoid.mempty else
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 26) Data.Monoid.<>
                          (((\ bs ->
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Prelude.. Data.Text.Encoding.encodeUtf8)
                            _v)
                     Data.Monoid.<>
                     (let _v
                            = Lens.Family2.view (Data.ProtoLens.Field.field @"meshPwd") _x
                        in
                        if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty else
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 34) Data.Monoid.<>
                            (((\ bs ->
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Prelude.. Data.Text.Encoding.encodeUtf8)
                              _v)
                       Data.Monoid.<>
                       (let _v
                              = Lens.Family2.view (Data.ProtoLens.Field.field @"mqttBroker") _x
                          in
                          if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                            Data.Monoid.mempty else
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 42) Data.Monoid.<>
                              (((\ bs ->
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Prelude.. Data.Text.Encoding.encodeUtf8)
                                _v)
                         Data.Monoid.<>
                         (let _v
                                = Lens.Family2.view
                                    (Data.ProtoLens.Field.field @"parentRssiThreshold")
                                    _x
                            in
                            if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                              Data.Monoid.mempty else
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 48) Data.Monoid.<>
                                ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                                   Prelude.fromIntegral)
                                  _v)
                           Data.Monoid.<>
                           (let _v
                                  = Lens.Family2.view
                                      (Data.ProtoLens.Field.field @"parenJoiningRssi")
                                      _x
                              in
                              if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty else
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 56) Data.Monoid.<>
                                  ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                                     Prelude.fromIntegral)
                                    _v)
                             Data.Monoid.<>
                             (let _v
                                    = Lens.Family2.view
                                        (Data.ProtoLens.Field.field @"maxChildNodesPerLayer")
                                        _x
                                in
                                if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                                  Data.Monoid.mempty else
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 64) Data.Monoid.<>
                                    ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                                       Prelude.fromIntegral)
                                      _v)
                               Data.Monoid.<>
                               Data.ProtoLens.Encoding.Wire.buildFieldSet
                                 (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData MeshConfig where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_MeshConfig'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_MeshConfig'wifiUname x__)
                    (Control.DeepSeq.deepseq (_MeshConfig'wifiPwd x__)
                       (Control.DeepSeq.deepseq (_MeshConfig'meshName x__)
                          (Control.DeepSeq.deepseq (_MeshConfig'meshPwd x__)
                             (Control.DeepSeq.deepseq (_MeshConfig'mqttBroker x__)
                                (Control.DeepSeq.deepseq (_MeshConfig'parentRssiThreshold x__)
                                   (Control.DeepSeq.deepseq (_MeshConfig'parenJoiningRssi x__)
                                      (Control.DeepSeq.deepseq
                                         (_MeshConfig'maxChildNodesPerLayer x__)
                                         (()))))))))))
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.time' @:: Lens' MeshFrame Data.Word.Word64@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'payload' @:: Lens' MeshFrame (Prelude.Maybe MeshFrame'Payload)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'control' @:: Lens' MeshFrame (Prelude.Maybe NodeControl)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.control' @:: Lens' MeshFrame NodeControl@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'state' @:: Lens' MeshFrame (Prelude.Maybe EnergyState)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.state' @:: Lens' MeshFrame EnergyState@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'transaction' @:: Lens' MeshFrame (Prelude.Maybe Transaction)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.transaction' @:: Lens' MeshFrame Transaction@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'transactionStatus' @:: Lens' MeshFrame (Prelude.Maybe EnergyTransactionStatus)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.transactionStatus' @:: Lens' MeshFrame EnergyTransactionStatus@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'hw' @:: Lens' MeshFrame (Prelude.Maybe HardwareConfig)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.hw' @:: Lens' MeshFrame HardwareConfig@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'rtStats' @:: Lens' MeshFrame (Prelude.Maybe RuntimeStats)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.rtStats' @:: Lens' MeshFrame RuntimeStats@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'meshConf' @:: Lens' MeshFrame (Prelude.Maybe MeshConfig)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.meshConf' @:: Lens' MeshFrame MeshConfig@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'otaConf' @:: Lens' MeshFrame (Prelude.Maybe OTAConfig)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.otaConf' @:: Lens' MeshFrame OTAConfig@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'meshversion' @:: Lens' MeshFrame (Prelude.Maybe SetVersion)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.meshversion' @:: Lens' MeshFrame SetVersion@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'parent' @:: Lens' MeshFrame (Prelude.Maybe ReconciliationParent)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.parent' @:: Lens' MeshFrame ReconciliationParent@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'child' @:: Lens' MeshFrame (Prelude.Maybe ReconciliationChild)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.child' @:: Lens' MeshFrame ReconciliationChild@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'forcedActions' @:: Lens' MeshFrame (Prelude.Maybe Actions)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.forcedActions' @:: Lens' MeshFrame Actions@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'otastatus' @:: Lens' MeshFrame (Prelude.Maybe UpdateStatus)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.otastatus' @:: Lens' MeshFrame UpdateStatus@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'nodeTxRequest' @:: Lens' MeshFrame (Prelude.Maybe EnergyTransactionRequest)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.nodeTxRequest' @:: Lens' MeshFrame EnergyTransactionRequest@
 -}
data MeshFrame = MeshFrame{_MeshFrame'time :: !Data.Word.Word64,
                           _MeshFrame'payload :: !(Prelude.Maybe MeshFrame'Payload),
                           _MeshFrame'_unknownFields :: !Data.ProtoLens.FieldSet}
                   deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show MeshFrame where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
data MeshFrame'Payload = MeshFrame'Control !NodeControl
                       | MeshFrame'State !EnergyState
                       | MeshFrame'Transaction !Transaction
                       | MeshFrame'TransactionStatus !EnergyTransactionStatus
                       | MeshFrame'Hw !HardwareConfig
                       | MeshFrame'RtStats !RuntimeStats
                       | MeshFrame'MeshConf !MeshConfig
                       | MeshFrame'OtaConf !OTAConfig
                       | MeshFrame'Meshversion !SetVersion
                       | MeshFrame'Parent !ReconciliationParent
                       | MeshFrame'Child !ReconciliationChild
                       | MeshFrame'ForcedActions !Actions
                       | MeshFrame'Otastatus !UpdateStatus
                       | MeshFrame'NodeTxRequest !EnergyTransactionRequest
                           deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField MeshFrame "time"
           (Data.Word.Word64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'time
               (\ x__ y__ -> x__{_MeshFrame'time = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField MeshFrame "maybe'payload"
           (Prelude.Maybe MeshFrame'Payload)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField MeshFrame "maybe'control"
           (Prelude.Maybe NodeControl)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MeshFrame'Control x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MeshFrame'Control y__)
instance Data.ProtoLens.Field.HasField MeshFrame "control"
           (NodeControl)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MeshFrame'Control x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MeshFrame'Control y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField MeshFrame "maybe'state"
           (Prelude.Maybe EnergyState)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MeshFrame'State x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MeshFrame'State y__)
instance Data.ProtoLens.Field.HasField MeshFrame "state"
           (EnergyState)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MeshFrame'State x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MeshFrame'State y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField MeshFrame
           "maybe'transaction"
           (Prelude.Maybe Transaction)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MeshFrame'Transaction x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MeshFrame'Transaction y__)
instance Data.ProtoLens.Field.HasField MeshFrame "transaction"
           (Transaction)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MeshFrame'Transaction x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MeshFrame'Transaction y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField MeshFrame
           "maybe'transactionStatus"
           (Prelude.Maybe EnergyTransactionStatus)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MeshFrame'TransactionStatus x__val) -> Prelude.Just
                                                                              x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MeshFrame'TransactionStatus y__)
instance Data.ProtoLens.Field.HasField MeshFrame
           "transactionStatus"
           (EnergyTransactionStatus)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MeshFrame'TransactionStatus x__val) -> Prelude.Just
                                                                               x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MeshFrame'TransactionStatus y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField MeshFrame "maybe'hw"
           (Prelude.Maybe HardwareConfig)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MeshFrame'Hw x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MeshFrame'Hw y__)
instance Data.ProtoLens.Field.HasField MeshFrame "hw"
           (HardwareConfig)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MeshFrame'Hw x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MeshFrame'Hw y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField MeshFrame "maybe'rtStats"
           (Prelude.Maybe RuntimeStats)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MeshFrame'RtStats x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MeshFrame'RtStats y__)
instance Data.ProtoLens.Field.HasField MeshFrame "rtStats"
           (RuntimeStats)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MeshFrame'RtStats x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MeshFrame'RtStats y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField MeshFrame "maybe'meshConf"
           (Prelude.Maybe MeshConfig)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MeshFrame'MeshConf x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MeshFrame'MeshConf y__)
instance Data.ProtoLens.Field.HasField MeshFrame "meshConf"
           (MeshConfig)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MeshFrame'MeshConf x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MeshFrame'MeshConf y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField MeshFrame "maybe'otaConf"
           (Prelude.Maybe OTAConfig)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MeshFrame'OtaConf x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MeshFrame'OtaConf y__)
instance Data.ProtoLens.Field.HasField MeshFrame "otaConf"
           (OTAConfig)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MeshFrame'OtaConf x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MeshFrame'OtaConf y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField MeshFrame
           "maybe'meshversion"
           (Prelude.Maybe SetVersion)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MeshFrame'Meshversion x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MeshFrame'Meshversion y__)
instance Data.ProtoLens.Field.HasField MeshFrame "meshversion"
           (SetVersion)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MeshFrame'Meshversion x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MeshFrame'Meshversion y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField MeshFrame "maybe'parent"
           (Prelude.Maybe ReconciliationParent)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MeshFrame'Parent x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MeshFrame'Parent y__)
instance Data.ProtoLens.Field.HasField MeshFrame "parent"
           (ReconciliationParent)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MeshFrame'Parent x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MeshFrame'Parent y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField MeshFrame "maybe'child"
           (Prelude.Maybe ReconciliationChild)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MeshFrame'Child x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MeshFrame'Child y__)
instance Data.ProtoLens.Field.HasField MeshFrame "child"
           (ReconciliationChild)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MeshFrame'Child x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MeshFrame'Child y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField MeshFrame
           "maybe'forcedActions"
           (Prelude.Maybe Actions)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MeshFrame'ForcedActions x__val) -> Prelude.Just
                                                                          x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MeshFrame'ForcedActions y__)
instance Data.ProtoLens.Field.HasField MeshFrame "forcedActions"
           (Actions)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MeshFrame'ForcedActions x__val) -> Prelude.Just
                                                                           x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MeshFrame'ForcedActions y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField MeshFrame "maybe'otastatus"
           (Prelude.Maybe UpdateStatus)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MeshFrame'Otastatus x__val) -> Prelude.Just x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MeshFrame'Otastatus y__)
instance Data.ProtoLens.Field.HasField MeshFrame "otastatus"
           (UpdateStatus)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MeshFrame'Otastatus x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MeshFrame'Otastatus y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField MeshFrame
           "maybe'nodeTxRequest"
           (Prelude.Maybe EnergyTransactionRequest)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens
                (\ x__ ->
                   case x__ of
                       Prelude.Just (MeshFrame'NodeTxRequest x__val) -> Prelude.Just
                                                                          x__val
                       _otherwise -> Prelude.Nothing)
                (\ _ y__ -> Prelude.fmap MeshFrame'NodeTxRequest y__)
instance Data.ProtoLens.Field.HasField MeshFrame "nodeTxRequest"
           (EnergyTransactionRequest)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _MeshFrame'payload
               (\ x__ y__ -> x__{_MeshFrame'payload = y__}))
              Prelude..
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (MeshFrame'NodeTxRequest x__val) -> Prelude.Just
                                                                           x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap MeshFrame'NodeTxRequest y__))
                Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Message MeshFrame where
        messageName _ = Data.Text.pack "MeshFrame"
        fieldsByTag
          = let time__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "time"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"time"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
                control__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "control"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor NodeControl)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'control"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
                state__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "state"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor EnergyState)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'state"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
                transaction__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "transaction"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Transaction)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'transaction"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
                transactionStatus__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "transactionStatus"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor EnergyTransactionStatus)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'transactionStatus"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
                hw__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "hw"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor HardwareConfig)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'hw"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
                rtStats__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "rtStats"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor RuntimeStats)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'rtStats"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
                meshConf__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "meshConf"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor MeshConfig)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'meshConf"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
                otaConf__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "otaConf"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor OTAConfig)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'otaConf"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
                meshversion__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "meshversion"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor SetVersion)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'meshversion"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
                parent__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "parent"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor ReconciliationParent)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'parent"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
                child__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "child"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor ReconciliationChild)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'child"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
                forcedActions__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "forced_actions"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Actions)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'forcedActions"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
                otastatus__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "otastatus"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor UpdateStatus)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'otastatus"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
                nodeTxRequest__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "nodeTxRequest"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor EnergyTransactionRequest)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'nodeTxRequest"))
                      :: Data.ProtoLens.FieldDescriptor MeshFrame
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, time__field_descriptor),
                 (Data.ProtoLens.Tag 2, control__field_descriptor),
                 (Data.ProtoLens.Tag 3, state__field_descriptor),
                 (Data.ProtoLens.Tag 4, transaction__field_descriptor),
                 (Data.ProtoLens.Tag 5, transactionStatus__field_descriptor),
                 (Data.ProtoLens.Tag 6, hw__field_descriptor),
                 (Data.ProtoLens.Tag 7, rtStats__field_descriptor),
                 (Data.ProtoLens.Tag 8, meshConf__field_descriptor),
                 (Data.ProtoLens.Tag 9, otaConf__field_descriptor),
                 (Data.ProtoLens.Tag 10, meshversion__field_descriptor),
                 (Data.ProtoLens.Tag 11, parent__field_descriptor),
                 (Data.ProtoLens.Tag 12, child__field_descriptor),
                 (Data.ProtoLens.Tag 13, forcedActions__field_descriptor),
                 (Data.ProtoLens.Tag 14, otastatus__field_descriptor),
                 (Data.ProtoLens.Tag 15, nodeTxRequest__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _MeshFrame'_unknownFields
              (\ x__ y__ -> x__{_MeshFrame'_unknownFields = y__})
        defMessage
          = MeshFrame{_MeshFrame'time = Data.ProtoLens.fieldDefault,
                      _MeshFrame'payload = Prelude.Nothing,
                      _MeshFrame'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     MeshFrame -> Data.ProtoLens.Encoding.Bytes.Parser MeshFrame
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                8 -> do y <- (Data.ProtoLens.Encoding.Bytes.getVarInt)
                                               Data.ProtoLens.Encoding.Bytes.<?> "time"
                                        loop
                                          (Lens.Family2.set (Data.ProtoLens.Field.field @"time") y
                                             x)
                                18 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "control"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"control")
                                              y
                                              x)
                                26 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "state"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"state") y
                                              x)
                                34 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "transaction"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"transaction")
                                              y
                                              x)
                                42 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "transactionStatus"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"transactionStatus")
                                              y
                                              x)
                                50 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "hw"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"hw") y x)
                                58 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "rtStats"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"rtStats")
                                              y
                                              x)
                                66 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "meshConf"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"meshConf")
                                              y
                                              x)
                                74 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "otaConf"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"otaConf")
                                              y
                                              x)
                                82 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "meshversion"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"meshversion")
                                              y
                                              x)
                                90 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "parent"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"parent")
                                              y
                                              x)
                                98 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "child"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"child") y
                                              x)
                                106 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "forced_actions"
                                          loop
                                            (Lens.Family2.set
                                               (Data.ProtoLens.Field.field @"forcedActions")
                                               y
                                               x)
                                114 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "otastatus"
                                          loop
                                            (Lens.Family2.set
                                               (Data.ProtoLens.Field.field @"otastatus")
                                               y
                                               x)
                                122 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "nodeTxRequest"
                                          loop
                                            (Lens.Family2.set
                                               (Data.ProtoLens.Field.field @"nodeTxRequest")
                                               y
                                               x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "MeshFrame"
        buildMessage
          = (\ _x ->
               (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"time") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 8) Data.Monoid.<>
                      Data.ProtoLens.Encoding.Bytes.putVarInt _v)
                 Data.Monoid.<>
                 (case
                    Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'payload") _x
                    of
                      (Prelude.Nothing) -> Data.Monoid.mempty
                      Prelude.Just
                        (MeshFrame'Control v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    18)
                                                   Data.Monoid.<>
                                                   (((\ bs ->
                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                           (Prelude.fromIntegral
                                                              (Data.ByteString.length bs)))
                                                          Data.Monoid.<>
                                                          Data.ProtoLens.Encoding.Bytes.putBytes
                                                            bs))
                                                      Prelude.. Data.ProtoLens.encodeMessage)
                                                     v
                      Prelude.Just
                        (MeshFrame'State v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                                                 Data.Monoid.<>
                                                 (((\ bs ->
                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                         (Prelude.fromIntegral
                                                            (Data.ByteString.length bs)))
                                                        Data.Monoid.<>
                                                        Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                                    Prelude.. Data.ProtoLens.encodeMessage)
                                                   v
                      Prelude.Just
                        (MeshFrame'Transaction
                           v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 34) Data.Monoid.<>
                                   (((\ bs ->
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Prelude.. Data.ProtoLens.encodeMessage)
                                     v
                      Prelude.Just
                        (MeshFrame'TransactionStatus
                           v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 42) Data.Monoid.<>
                                   (((\ bs ->
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Prelude.. Data.ProtoLens.encodeMessage)
                                     v
                      Prelude.Just
                        (MeshFrame'Hw v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                                              Data.Monoid.<>
                                              (((\ bs ->
                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                      (Prelude.fromIntegral
                                                         (Data.ByteString.length bs)))
                                                     Data.Monoid.<>
                                                     Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                                 Prelude.. Data.ProtoLens.encodeMessage)
                                                v
                      Prelude.Just
                        (MeshFrame'RtStats v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    58)
                                                   Data.Monoid.<>
                                                   (((\ bs ->
                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                           (Prelude.fromIntegral
                                                              (Data.ByteString.length bs)))
                                                          Data.Monoid.<>
                                                          Data.ProtoLens.Encoding.Bytes.putBytes
                                                            bs))
                                                      Prelude.. Data.ProtoLens.encodeMessage)
                                                     v
                      Prelude.Just
                        (MeshFrame'MeshConf v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                     66)
                                                    Data.Monoid.<>
                                                    (((\ bs ->
                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                            (Prelude.fromIntegral
                                                               (Data.ByteString.length bs)))
                                                           Data.Monoid.<>
                                                           Data.ProtoLens.Encoding.Bytes.putBytes
                                                             bs))
                                                       Prelude.. Data.ProtoLens.encodeMessage)
                                                      v
                      Prelude.Just
                        (MeshFrame'OtaConf v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    74)
                                                   Data.Monoid.<>
                                                   (((\ bs ->
                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                           (Prelude.fromIntegral
                                                              (Data.ByteString.length bs)))
                                                          Data.Monoid.<>
                                                          Data.ProtoLens.Encoding.Bytes.putBytes
                                                            bs))
                                                      Prelude.. Data.ProtoLens.encodeMessage)
                                                     v
                      Prelude.Just
                        (MeshFrame'Meshversion
                           v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 82) Data.Monoid.<>
                                   (((\ bs ->
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Prelude.. Data.ProtoLens.encodeMessage)
                                     v
                      Prelude.Just
                        (MeshFrame'Parent v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                   90)
                                                  Data.Monoid.<>
                                                  (((\ bs ->
                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                          (Prelude.fromIntegral
                                                             (Data.ByteString.length bs)))
                                                         Data.Monoid.<>
                                                         Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                                     Prelude.. Data.ProtoLens.encodeMessage)
                                                    v
                      Prelude.Just
                        (MeshFrame'Child v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 98)
                                                 Data.Monoid.<>
                                                 (((\ bs ->
                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                         (Prelude.fromIntegral
                                                            (Data.ByteString.length bs)))
                                                        Data.Monoid.<>
                                                        Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                                    Prelude.. Data.ProtoLens.encodeMessage)
                                                   v
                      Prelude.Just
                        (MeshFrame'ForcedActions
                           v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 106) Data.Monoid.<>
                                   (((\ bs ->
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Prelude.. Data.ProtoLens.encodeMessage)
                                     v
                      Prelude.Just
                        (MeshFrame'Otastatus v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                      114)
                                                     Data.Monoid.<>
                                                     (((\ bs ->
                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             (Prelude.fromIntegral
                                                                (Data.ByteString.length bs)))
                                                            Data.Monoid.<>
                                                            Data.ProtoLens.Encoding.Bytes.putBytes
                                                              bs))
                                                        Prelude.. Data.ProtoLens.encodeMessage)
                                                       v
                      Prelude.Just
                        (MeshFrame'NodeTxRequest
                           v) -> (Data.ProtoLens.Encoding.Bytes.putVarInt 122) Data.Monoid.<>
                                   (((\ bs ->
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Prelude.. Data.ProtoLens.encodeMessage)
                                     v)
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData MeshFrame where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_MeshFrame'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_MeshFrame'time x__)
                    (Control.DeepSeq.deepseq (_MeshFrame'payload x__) (()))))
instance Control.DeepSeq.NFData MeshFrame'Payload where
        rnf (MeshFrame'Control x__) = Control.DeepSeq.rnf x__
        rnf (MeshFrame'State x__) = Control.DeepSeq.rnf x__
        rnf (MeshFrame'Transaction x__) = Control.DeepSeq.rnf x__
        rnf (MeshFrame'TransactionStatus x__) = Control.DeepSeq.rnf x__
        rnf (MeshFrame'Hw x__) = Control.DeepSeq.rnf x__
        rnf (MeshFrame'RtStats x__) = Control.DeepSeq.rnf x__
        rnf (MeshFrame'MeshConf x__) = Control.DeepSeq.rnf x__
        rnf (MeshFrame'OtaConf x__) = Control.DeepSeq.rnf x__
        rnf (MeshFrame'Meshversion x__) = Control.DeepSeq.rnf x__
        rnf (MeshFrame'Parent x__) = Control.DeepSeq.rnf x__
        rnf (MeshFrame'Child x__) = Control.DeepSeq.rnf x__
        rnf (MeshFrame'ForcedActions x__) = Control.DeepSeq.rnf x__
        rnf (MeshFrame'Otastatus x__) = Control.DeepSeq.rnf x__
        rnf (MeshFrame'NodeTxRequest x__) = Control.DeepSeq.rnf x__
_MeshFrame'Control ::
                   Data.ProtoLens.Prism.Prism' MeshFrame'Payload NodeControl
_MeshFrame'Control
  = Data.ProtoLens.Prism.prism' MeshFrame'Control
      (\ p__ ->
         case p__ of
             MeshFrame'Control p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_MeshFrame'State ::
                 Data.ProtoLens.Prism.Prism' MeshFrame'Payload EnergyState
_MeshFrame'State
  = Data.ProtoLens.Prism.prism' MeshFrame'State
      (\ p__ ->
         case p__ of
             MeshFrame'State p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_MeshFrame'Transaction ::
                       Data.ProtoLens.Prism.Prism' MeshFrame'Payload Transaction
_MeshFrame'Transaction
  = Data.ProtoLens.Prism.prism' MeshFrame'Transaction
      (\ p__ ->
         case p__ of
             MeshFrame'Transaction p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_MeshFrame'TransactionStatus ::
                             Data.ProtoLens.Prism.Prism' MeshFrame'Payload
                               EnergyTransactionStatus
_MeshFrame'TransactionStatus
  = Data.ProtoLens.Prism.prism' MeshFrame'TransactionStatus
      (\ p__ ->
         case p__ of
             MeshFrame'TransactionStatus p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_MeshFrame'Hw ::
              Data.ProtoLens.Prism.Prism' MeshFrame'Payload HardwareConfig
_MeshFrame'Hw
  = Data.ProtoLens.Prism.prism' MeshFrame'Hw
      (\ p__ ->
         case p__ of
             MeshFrame'Hw p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_MeshFrame'RtStats ::
                   Data.ProtoLens.Prism.Prism' MeshFrame'Payload RuntimeStats
_MeshFrame'RtStats
  = Data.ProtoLens.Prism.prism' MeshFrame'RtStats
      (\ p__ ->
         case p__ of
             MeshFrame'RtStats p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_MeshFrame'MeshConf ::
                    Data.ProtoLens.Prism.Prism' MeshFrame'Payload MeshConfig
_MeshFrame'MeshConf
  = Data.ProtoLens.Prism.prism' MeshFrame'MeshConf
      (\ p__ ->
         case p__ of
             MeshFrame'MeshConf p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_MeshFrame'OtaConf ::
                   Data.ProtoLens.Prism.Prism' MeshFrame'Payload OTAConfig
_MeshFrame'OtaConf
  = Data.ProtoLens.Prism.prism' MeshFrame'OtaConf
      (\ p__ ->
         case p__ of
             MeshFrame'OtaConf p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_MeshFrame'Meshversion ::
                       Data.ProtoLens.Prism.Prism' MeshFrame'Payload SetVersion
_MeshFrame'Meshversion
  = Data.ProtoLens.Prism.prism' MeshFrame'Meshversion
      (\ p__ ->
         case p__ of
             MeshFrame'Meshversion p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_MeshFrame'Parent ::
                  Data.ProtoLens.Prism.Prism' MeshFrame'Payload ReconciliationParent
_MeshFrame'Parent
  = Data.ProtoLens.Prism.prism' MeshFrame'Parent
      (\ p__ ->
         case p__ of
             MeshFrame'Parent p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_MeshFrame'Child ::
                 Data.ProtoLens.Prism.Prism' MeshFrame'Payload ReconciliationChild
_MeshFrame'Child
  = Data.ProtoLens.Prism.prism' MeshFrame'Child
      (\ p__ ->
         case p__ of
             MeshFrame'Child p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_MeshFrame'ForcedActions ::
                         Data.ProtoLens.Prism.Prism' MeshFrame'Payload Actions
_MeshFrame'ForcedActions
  = Data.ProtoLens.Prism.prism' MeshFrame'ForcedActions
      (\ p__ ->
         case p__ of
             MeshFrame'ForcedActions p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_MeshFrame'Otastatus ::
                     Data.ProtoLens.Prism.Prism' MeshFrame'Payload UpdateStatus
_MeshFrame'Otastatus
  = Data.ProtoLens.Prism.prism' MeshFrame'Otastatus
      (\ p__ ->
         case p__ of
             MeshFrame'Otastatus p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_MeshFrame'NodeTxRequest ::
                         Data.ProtoLens.Prism.Prism' MeshFrame'Payload
                           EnergyTransactionRequest
_MeshFrame'NodeTxRequest
  = Data.ProtoLens.Prism.prism' MeshFrame'NodeTxRequest
      (\ p__ ->
         case p__ of
             MeshFrame'NodeTxRequest p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.disconnectGrid' @:: Lens' NodeControl Prelude.Bool@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.disconnectLoad' @:: Lens' NodeControl Prelude.Bool@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.disconnectSolar' @:: Lens' NodeControl Prelude.Bool@
 -}
data NodeControl = NodeControl{_NodeControl'disconnectGrid ::
                               !Prelude.Bool,
                               _NodeControl'disconnectLoad :: !Prelude.Bool,
                               _NodeControl'disconnectSolar :: !Prelude.Bool,
                               _NodeControl'_unknownFields :: !Data.ProtoLens.FieldSet}
                     deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show NodeControl where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField NodeControl "disconnectGrid"
           (Prelude.Bool)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _NodeControl'disconnectGrid
               (\ x__ y__ -> x__{_NodeControl'disconnectGrid = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField NodeControl "disconnectLoad"
           (Prelude.Bool)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _NodeControl'disconnectLoad
               (\ x__ y__ -> x__{_NodeControl'disconnectLoad = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField NodeControl
           "disconnectSolar"
           (Prelude.Bool)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _NodeControl'disconnectSolar
               (\ x__ y__ -> x__{_NodeControl'disconnectSolar = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message NodeControl where
        messageName _ = Data.Text.pack "NodeControl"
        fieldsByTag
          = let disconnectGrid__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "disconnectGrid"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"disconnectGrid"))
                      :: Data.ProtoLens.FieldDescriptor NodeControl
                disconnectLoad__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "disconnectLoad"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"disconnectLoad"))
                      :: Data.ProtoLens.FieldDescriptor NodeControl
                disconnectSolar__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "disconnectSolar"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"disconnectSolar"))
                      :: Data.ProtoLens.FieldDescriptor NodeControl
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, disconnectGrid__field_descriptor),
                 (Data.ProtoLens.Tag 2, disconnectLoad__field_descriptor),
                 (Data.ProtoLens.Tag 3, disconnectSolar__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _NodeControl'_unknownFields
              (\ x__ y__ -> x__{_NodeControl'_unknownFields = y__})
        defMessage
          = NodeControl{_NodeControl'disconnectGrid =
                          Data.ProtoLens.fieldDefault,
                        _NodeControl'disconnectLoad = Data.ProtoLens.fieldDefault,
                        _NodeControl'disconnectSolar = Data.ProtoLens.fieldDefault,
                        _NodeControl'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     NodeControl -> Data.ProtoLens.Encoding.Bytes.Parser NodeControl
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                8 -> do y <- (Prelude.fmap ((Prelude./=) 0)
                                                Data.ProtoLens.Encoding.Bytes.getVarInt)
                                               Data.ProtoLens.Encoding.Bytes.<?> "disconnectGrid"
                                        loop
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"disconnectGrid")
                                             y
                                             x)
                                16 -> do y <- (Prelude.fmap ((Prelude./=) 0)
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "disconnectLoad"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"disconnectLoad")
                                              y
                                              x)
                                24 -> do y <- (Prelude.fmap ((Prelude./=) 0)
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "disconnectSolar"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"disconnectSolar")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "NodeControl"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"disconnectGrid")
                          _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 8) Data.Monoid.<>
                      ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                         (\ b -> if b then 1 else 0))
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"disconnectLoad")
                            _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 16) Data.Monoid.<>
                        ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                           (\ b -> if b then 1 else 0))
                          _v)
                   Data.Monoid.<>
                   (let _v
                          = Lens.Family2.view (Data.ProtoLens.Field.field @"disconnectSolar")
                              _x
                      in
                      if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                        Data.Monoid.mempty else
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 24) Data.Monoid.<>
                          ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                             (\ b -> if b then 1 else 0))
                            _v)
                     Data.Monoid.<>
                     Data.ProtoLens.Encoding.Wire.buildFieldSet
                       (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData NodeControl where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_NodeControl'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_NodeControl'disconnectGrid x__)
                    (Control.DeepSeq.deepseq (_NodeControl'disconnectLoad x__)
                       (Control.DeepSeq.deepseq (_NodeControl'disconnectSolar x__)
                          (())))))
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.macAddr' @:: Lens' NodeId Data.Text.Text@
 -}
data NodeId = NodeId{_NodeId'macAddr :: !Data.Text.Text,
                     _NodeId'_unknownFields :: !Data.ProtoLens.FieldSet}
                deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show NodeId where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField NodeId "macAddr"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _NodeId'macAddr
               (\ x__ y__ -> x__{_NodeId'macAddr = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message NodeId where
        messageName _ = Data.Text.pack "NodeId"
        fieldsByTag
          = let macAddr__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "macAddr"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"macAddr"))
                      :: Data.ProtoLens.FieldDescriptor NodeId
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, macAddr__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _NodeId'_unknownFields
              (\ x__ y__ -> x__{_NodeId'_unknownFields = y__})
        defMessage
          = NodeId{_NodeId'macAddr = Data.ProtoLens.fieldDefault,
                   _NodeId'_unknownFields = ([])}
        parseMessage
          = let loop :: NodeId -> Data.ProtoLens.Encoding.Bytes.Parser NodeId
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "macAddr"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"macAddr")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "NodeId"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"macAddr") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                      (((\ bs ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                             Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Prelude.. Data.Text.Encoding.encodeUtf8)
                        _v)
                 Data.Monoid.<>
                 Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData NodeId where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_NodeId'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_NodeId'macAddr x__) (())))
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.endpoint' @:: Lens' OTAConfig Data.Text.Text@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.timeOfDay' @:: Lens' OTAConfig Data.Word.Word32@
 -}
data OTAConfig = OTAConfig{_OTAConfig'endpoint :: !Data.Text.Text,
                           _OTAConfig'timeOfDay :: !Data.Word.Word32,
                           _OTAConfig'_unknownFields :: !Data.ProtoLens.FieldSet}
                   deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show OTAConfig where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField OTAConfig "endpoint"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _OTAConfig'endpoint
               (\ x__ y__ -> x__{_OTAConfig'endpoint = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField OTAConfig "timeOfDay"
           (Data.Word.Word32)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _OTAConfig'timeOfDay
               (\ x__ y__ -> x__{_OTAConfig'timeOfDay = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message OTAConfig where
        messageName _ = Data.Text.pack "OTAConfig"
        fieldsByTag
          = let endpoint__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "endpoint"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"endpoint"))
                      :: Data.ProtoLens.FieldDescriptor OTAConfig
                timeOfDay__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "timeOfDay"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"timeOfDay"))
                      :: Data.ProtoLens.FieldDescriptor OTAConfig
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, endpoint__field_descriptor),
                 (Data.ProtoLens.Tag 2, timeOfDay__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _OTAConfig'_unknownFields
              (\ x__ y__ -> x__{_OTAConfig'_unknownFields = y__})
        defMessage
          = OTAConfig{_OTAConfig'endpoint = Data.ProtoLens.fieldDefault,
                      _OTAConfig'timeOfDay = Data.ProtoLens.fieldDefault,
                      _OTAConfig'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     OTAConfig -> Data.ProtoLens.Encoding.Bytes.Parser OTAConfig
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "endpoint"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"endpoint")
                                              y
                                              x)
                                16 -> do y <- (Prelude.fmap Prelude.fromIntegral
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "timeOfDay"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"timeOfDay")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "OTAConfig"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"endpoint") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                      (((\ bs ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                             Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Prelude.. Data.Text.Encoding.encodeUtf8)
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"timeOfDay") _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 16) Data.Monoid.<>
                        ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                           Prelude.fromIntegral)
                          _v)
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData OTAConfig where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_OTAConfig'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_OTAConfig'endpoint x__)
                    (Control.DeepSeq.deepseq (_OTAConfig'timeOfDay x__) (()))))
newtype PDirection'UnrecognizedValue = PDirection'UnrecognizedValue Data.Int.Int32
                                         deriving (Prelude.Eq, Prelude.Ord, Prelude.Show)
data PDirection = Incoming
                | Outgoing
                | Load
                | PDirection'Unrecognized !PDirection'UnrecognizedValue
                    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum PDirection where
        maybeToEnum 0 = Prelude.Just Incoming
        maybeToEnum 1 = Prelude.Just Outgoing
        maybeToEnum 2 = Prelude.Just Load
        maybeToEnum k
          = Prelude.Just
              (PDirection'Unrecognized
                 (PDirection'UnrecognizedValue (Prelude.fromIntegral k)))
        showEnum Incoming = "incoming"
        showEnum Outgoing = "outgoing"
        showEnum Load = "load"
        showEnum (PDirection'Unrecognized (PDirection'UnrecognizedValue k))
          = Prelude.show k
        readEnum k
          | (k) Prelude.== "incoming" = Prelude.Just Incoming
          | (k) Prelude.== "outgoing" = Prelude.Just Outgoing
          | (k) Prelude.== "load" = Prelude.Just Load
        readEnum k
          = (Text.Read.readMaybe k) Prelude.>>= Data.ProtoLens.maybeToEnum
instance Prelude.Bounded PDirection where
        minBound = Incoming
        maxBound = Load
instance Prelude.Enum PDirection where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 (("toEnum: unknown value for enum PDirection: ") Prelude.++
                    Prelude.show k__))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum Incoming = 0
        fromEnum Outgoing = 1
        fromEnum Load = 2
        fromEnum (PDirection'Unrecognized (PDirection'UnrecognizedValue k))
          = Prelude.fromIntegral k
        succ Load
          = Prelude.error
              "PDirection.succ: bad argument Load. This value would be out of bounds."
        succ Incoming = Outgoing
        succ Outgoing = Load
        succ (PDirection'Unrecognized _)
          = Prelude.error "PDirection.succ: bad argument: unrecognized value"
        pred Incoming
          = Prelude.error
              "PDirection.pred: bad argument Incoming. This value would be out of bounds."
        pred Outgoing = Incoming
        pred Load = Outgoing
        pred (PDirection'Unrecognized _)
          = Prelude.error "PDirection.pred: bad argument: unrecognized value"
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault PDirection where
        fieldDefault = Incoming
instance Control.DeepSeq.NFData PDirection where
        rnf x__ = Prelude.seq x__ (())
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.vOC' @:: Lens' PVParameters Prelude.Float@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.vMPPT' @:: Lens' PVParameters Prelude.Float@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.iMPPT' @:: Lens' PVParameters Prelude.Float@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.ratedPower' @:: Lens' PVParameters Prelude.Float@
 -}
data PVParameters = PVParameters{_PVParameters'vOC ::
                                 !Prelude.Float,
                                 _PVParameters'vMPPT :: !Prelude.Float,
                                 _PVParameters'iMPPT :: !Prelude.Float,
                                 _PVParameters'ratedPower :: !Prelude.Float,
                                 _PVParameters'_unknownFields :: !Data.ProtoLens.FieldSet}
                      deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PVParameters where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField PVParameters "vOC"
           (Prelude.Float)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _PVParameters'vOC
               (\ x__ y__ -> x__{_PVParameters'vOC = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField PVParameters "vMPPT"
           (Prelude.Float)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _PVParameters'vMPPT
               (\ x__ y__ -> x__{_PVParameters'vMPPT = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField PVParameters "iMPPT"
           (Prelude.Float)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _PVParameters'iMPPT
               (\ x__ y__ -> x__{_PVParameters'iMPPT = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField PVParameters "ratedPower"
           (Prelude.Float)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _PVParameters'ratedPower
               (\ x__ y__ -> x__{_PVParameters'ratedPower = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message PVParameters where
        messageName _ = Data.Text.pack "PVParameters"
        fieldsByTag
          = let vOC__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vOC"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"vOC"))
                      :: Data.ProtoLens.FieldDescriptor PVParameters
                vMPPT__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vMPPT"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"vMPPT"))
                      :: Data.ProtoLens.FieldDescriptor PVParameters
                iMPPT__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "iMPPT"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"iMPPT"))
                      :: Data.ProtoLens.FieldDescriptor PVParameters
                ratedPower__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "ratedPower"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.FloatField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Float)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"ratedPower"))
                      :: Data.ProtoLens.FieldDescriptor PVParameters
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, vOC__field_descriptor),
                 (Data.ProtoLens.Tag 2, vMPPT__field_descriptor),
                 (Data.ProtoLens.Tag 3, iMPPT__field_descriptor),
                 (Data.ProtoLens.Tag 4, ratedPower__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _PVParameters'_unknownFields
              (\ x__ y__ -> x__{_PVParameters'_unknownFields = y__})
        defMessage
          = PVParameters{_PVParameters'vOC = Data.ProtoLens.fieldDefault,
                         _PVParameters'vMPPT = Data.ProtoLens.fieldDefault,
                         _PVParameters'iMPPT = Data.ProtoLens.fieldDefault,
                         _PVParameters'ratedPower = Data.ProtoLens.fieldDefault,
                         _PVParameters'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     PVParameters -> Data.ProtoLens.Encoding.Bytes.Parser PVParameters
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                13 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToFloat
                                                 Data.ProtoLens.Encoding.Bytes.getFixed32)
                                                Data.ProtoLens.Encoding.Bytes.<?> "vOC"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"vOC") y
                                              x)
                                21 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToFloat
                                                 Data.ProtoLens.Encoding.Bytes.getFixed32)
                                                Data.ProtoLens.Encoding.Bytes.<?> "vMPPT"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"vMPPT") y
                                              x)
                                29 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToFloat
                                                 Data.ProtoLens.Encoding.Bytes.getFixed32)
                                                Data.ProtoLens.Encoding.Bytes.<?> "iMPPT"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"iMPPT") y
                                              x)
                                37 -> do y <- (Prelude.fmap
                                                 Data.ProtoLens.Encoding.Bytes.wordToFloat
                                                 Data.ProtoLens.Encoding.Bytes.getFixed32)
                                                Data.ProtoLens.Encoding.Bytes.<?> "ratedPower"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"ratedPower")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "PVParameters"
        buildMessage
          = (\ _x ->
               (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"vOC") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 13) Data.Monoid.<>
                      ((Data.ProtoLens.Encoding.Bytes.putFixed32) Prelude..
                         Data.ProtoLens.Encoding.Bytes.floatToWord)
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"vMPPT") _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 21) Data.Monoid.<>
                        ((Data.ProtoLens.Encoding.Bytes.putFixed32) Prelude..
                           Data.ProtoLens.Encoding.Bytes.floatToWord)
                          _v)
                   Data.Monoid.<>
                   (let _v
                          = Lens.Family2.view (Data.ProtoLens.Field.field @"iMPPT") _x
                      in
                      if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                        Data.Monoid.mempty else
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 29) Data.Monoid.<>
                          ((Data.ProtoLens.Encoding.Bytes.putFixed32) Prelude..
                             Data.ProtoLens.Encoding.Bytes.floatToWord)
                            _v)
                     Data.Monoid.<>
                     (let _v
                            = Lens.Family2.view (Data.ProtoLens.Field.field @"ratedPower") _x
                        in
                        if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty else
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 37) Data.Monoid.<>
                            ((Data.ProtoLens.Encoding.Bytes.putFixed32) Prelude..
                               Data.ProtoLens.Encoding.Bytes.floatToWord)
                              _v)
                       Data.Monoid.<>
                       Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData PVParameters where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_PVParameters'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_PVParameters'vOC x__)
                    (Control.DeepSeq.deepseq (_PVParameters'vMPPT x__)
                       (Control.DeepSeq.deepseq (_PVParameters'iMPPT x__)
                          (Control.DeepSeq.deepseq (_PVParameters'ratedPower x__) (()))))))
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.needsRecon' @:: Lens' ReconciliationChild Prelude.Bool@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.nodeMac' @:: Lens' ReconciliationChild Data.Text.Text@
 -}
data ReconciliationChild = ReconciliationChild{_ReconciliationChild'needsRecon
                                               :: !Prelude.Bool,
                                               _ReconciliationChild'nodeMac :: !Data.Text.Text,
                                               _ReconciliationChild'_unknownFields ::
                                               !Data.ProtoLens.FieldSet}
                             deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReconciliationChild where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReconciliationChild
           "needsRecon"
           (Prelude.Bool)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReconciliationChild'needsRecon
               (\ x__ y__ -> x__{_ReconciliationChild'needsRecon = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField ReconciliationChild
           "nodeMac"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReconciliationChild'nodeMac
               (\ x__ y__ -> x__{_ReconciliationChild'nodeMac = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message ReconciliationChild where
        messageName _ = Data.Text.pack "ReconciliationChild"
        fieldsByTag
          = let needsRecon__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "needsRecon"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"needsRecon"))
                      :: Data.ProtoLens.FieldDescriptor ReconciliationChild
                nodeMac__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "nodeMac"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"nodeMac"))
                      :: Data.ProtoLens.FieldDescriptor ReconciliationChild
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, needsRecon__field_descriptor),
                 (Data.ProtoLens.Tag 2, nodeMac__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _ReconciliationChild'_unknownFields
              (\ x__ y__ -> x__{_ReconciliationChild'_unknownFields = y__})
        defMessage
          = ReconciliationChild{_ReconciliationChild'needsRecon =
                                  Data.ProtoLens.fieldDefault,
                                _ReconciliationChild'nodeMac = Data.ProtoLens.fieldDefault,
                                _ReconciliationChild'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     ReconciliationChild ->
                       Data.ProtoLens.Encoding.Bytes.Parser ReconciliationChild
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                8 -> do y <- (Prelude.fmap ((Prelude./=) 0)
                                                Data.ProtoLens.Encoding.Bytes.getVarInt)
                                               Data.ProtoLens.Encoding.Bytes.<?> "needsRecon"
                                        loop
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"needsRecon")
                                             y
                                             x)
                                18 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "nodeMac"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"nodeMac")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "ReconciliationChild"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"needsRecon") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 8) Data.Monoid.<>
                      ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                         (\ b -> if b then 1 else 0))
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"nodeMac") _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 18) Data.Monoid.<>
                        (((\ bs ->
                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                (Prelude.fromIntegral (Data.ByteString.length bs)))
                               Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Prelude.. Data.Text.Encoding.encodeUtf8)
                          _v)
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ReconciliationChild where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_ReconciliationChild'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_ReconciliationChild'needsRecon x__)
                    (Control.DeepSeq.deepseq (_ReconciliationChild'nodeMac x__) (()))))
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.parentversion' @:: Lens' ReconciliationParent Data.Text.Text@
 -}
data ReconciliationParent = ReconciliationParent{_ReconciliationParent'parentversion
                                                 :: !Data.Text.Text,
                                                 _ReconciliationParent'_unknownFields ::
                                                 !Data.ProtoLens.FieldSet}
                              deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReconciliationParent where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReconciliationParent
           "parentversion"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _ReconciliationParent'parentversion
               (\ x__ y__ -> x__{_ReconciliationParent'parentversion = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message ReconciliationParent where
        messageName _ = Data.Text.pack "ReconciliationParent"
        fieldsByTag
          = let parentversion__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "parentversion"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"parentversion"))
                      :: Data.ProtoLens.FieldDescriptor ReconciliationParent
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, parentversion__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _ReconciliationParent'_unknownFields
              (\ x__ y__ -> x__{_ReconciliationParent'_unknownFields = y__})
        defMessage
          = ReconciliationParent{_ReconciliationParent'parentversion =
                                   Data.ProtoLens.fieldDefault,
                                 _ReconciliationParent'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     ReconciliationParent ->
                       Data.ProtoLens.Encoding.Bytes.Parser ReconciliationParent
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "parentversion"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"parentversion")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "ReconciliationParent"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"parentversion")
                          _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                      (((\ bs ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                             Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Prelude.. Data.Text.Encoding.encodeUtf8)
                        _v)
                 Data.Monoid.<>
                 Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ReconciliationParent where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_ReconciliationParent'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_ReconciliationParent'parentversion x__)
                    (())))
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.minFreeHeap' @:: Lens' RuntimeStats Data.Word.Word32@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.currentFreeHeap' @:: Lens' RuntimeStats Data.Word.Word32@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.cpuUtilization' @:: Lens' RuntimeStats Data.Word.Word32@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.isRoot' @:: Lens' RuntimeStats Prelude.Bool@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.children' @:: Lens' RuntimeStats [NodeId]@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.vec'children' @:: Lens' RuntimeStats (Data.Vector.Vector NodeId)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.wifiStrength' @:: Lens' RuntimeStats Data.Int.Int32@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.meshParentStrength' @:: Lens' RuntimeStats Data.Int.Int32@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.version' @:: Lens' RuntimeStats Data.Text.Text@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.uptime' @:: Lens' RuntimeStats Data.Word.Word64@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.parent' @:: Lens' RuntimeStats NodeId@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'parent' @:: Lens' RuntimeStats (Prelude.Maybe NodeId)@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.cpuTime' @:: Lens' RuntimeStats Data.Word.Word64@
 -}
data RuntimeStats = RuntimeStats{_RuntimeStats'minFreeHeap ::
                                 !Data.Word.Word32,
                                 _RuntimeStats'currentFreeHeap :: !Data.Word.Word32,
                                 _RuntimeStats'cpuUtilization :: !Data.Word.Word32,
                                 _RuntimeStats'isRoot :: !Prelude.Bool,
                                 _RuntimeStats'children :: !(Data.Vector.Vector NodeId),
                                 _RuntimeStats'wifiStrength :: !Data.Int.Int32,
                                 _RuntimeStats'meshParentStrength :: !Data.Int.Int32,
                                 _RuntimeStats'version :: !Data.Text.Text,
                                 _RuntimeStats'uptime :: !Data.Word.Word64,
                                 _RuntimeStats'parent :: !(Prelude.Maybe NodeId),
                                 _RuntimeStats'cpuTime :: !Data.Word.Word64,
                                 _RuntimeStats'_unknownFields :: !Data.ProtoLens.FieldSet}
                      deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show RuntimeStats where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField RuntimeStats "minFreeHeap"
           (Data.Word.Word32)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _RuntimeStats'minFreeHeap
               (\ x__ y__ -> x__{_RuntimeStats'minFreeHeap = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField RuntimeStats
           "currentFreeHeap"
           (Data.Word.Word32)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _RuntimeStats'currentFreeHeap
               (\ x__ y__ -> x__{_RuntimeStats'currentFreeHeap = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField RuntimeStats
           "cpuUtilization"
           (Data.Word.Word32)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _RuntimeStats'cpuUtilization
               (\ x__ y__ -> x__{_RuntimeStats'cpuUtilization = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField RuntimeStats "isRoot"
           (Prelude.Bool)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _RuntimeStats'isRoot
               (\ x__ y__ -> x__{_RuntimeStats'isRoot = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField RuntimeStats "children"
           ([NodeId])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _RuntimeStats'children
               (\ x__ y__ -> x__{_RuntimeStats'children = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField RuntimeStats "vec'children"
           (Data.Vector.Vector NodeId)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _RuntimeStats'children
               (\ x__ y__ -> x__{_RuntimeStats'children = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField RuntimeStats "wifiStrength"
           (Data.Int.Int32)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _RuntimeStats'wifiStrength
               (\ x__ y__ -> x__{_RuntimeStats'wifiStrength = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField RuntimeStats
           "meshParentStrength"
           (Data.Int.Int32)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _RuntimeStats'meshParentStrength
               (\ x__ y__ -> x__{_RuntimeStats'meshParentStrength = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField RuntimeStats "version"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _RuntimeStats'version
               (\ x__ y__ -> x__{_RuntimeStats'version = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField RuntimeStats "uptime"
           (Data.Word.Word64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _RuntimeStats'uptime
               (\ x__ y__ -> x__{_RuntimeStats'uptime = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField RuntimeStats "parent"
           (NodeId)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _RuntimeStats'parent
               (\ x__ y__ -> x__{_RuntimeStats'parent = y__}))
              Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField RuntimeStats "maybe'parent"
           (Prelude.Maybe NodeId)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _RuntimeStats'parent
               (\ x__ y__ -> x__{_RuntimeStats'parent = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField RuntimeStats "cpuTime"
           (Data.Word.Word64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _RuntimeStats'cpuTime
               (\ x__ y__ -> x__{_RuntimeStats'cpuTime = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message RuntimeStats where
        messageName _ = Data.Text.pack "RuntimeStats"
        fieldsByTag
          = let minFreeHeap__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "minFreeHeap"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"minFreeHeap"))
                      :: Data.ProtoLens.FieldDescriptor RuntimeStats
                currentFreeHeap__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "currentFreeHeap"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"currentFreeHeap"))
                      :: Data.ProtoLens.FieldDescriptor RuntimeStats
                cpuUtilization__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "cpuUtilization"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"cpuUtilization"))
                      :: Data.ProtoLens.FieldDescriptor RuntimeStats
                isRoot__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "isRoot"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"isRoot"))
                      :: Data.ProtoLens.FieldDescriptor RuntimeStats
                children__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "children"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor NodeId)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"children"))
                      :: Data.ProtoLens.FieldDescriptor RuntimeStats
                wifiStrength__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "wifiStrength"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"wifiStrength"))
                      :: Data.ProtoLens.FieldDescriptor RuntimeStats
                meshParentStrength__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "meshParentStrength"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"meshParentStrength"))
                      :: Data.ProtoLens.FieldDescriptor RuntimeStats
                version__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "version"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"version"))
                      :: Data.ProtoLens.FieldDescriptor RuntimeStats
                uptime__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "uptime"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"uptime"))
                      :: Data.ProtoLens.FieldDescriptor RuntimeStats
                parent__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "parent"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor NodeId)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'parent"))
                      :: Data.ProtoLens.FieldDescriptor RuntimeStats
                cpuTime__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "cpu_time"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"cpuTime"))
                      :: Data.ProtoLens.FieldDescriptor RuntimeStats
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, minFreeHeap__field_descriptor),
                 (Data.ProtoLens.Tag 2, currentFreeHeap__field_descriptor),
                 (Data.ProtoLens.Tag 3, cpuUtilization__field_descriptor),
                 (Data.ProtoLens.Tag 4, isRoot__field_descriptor),
                 (Data.ProtoLens.Tag 5, children__field_descriptor),
                 (Data.ProtoLens.Tag 6, wifiStrength__field_descriptor),
                 (Data.ProtoLens.Tag 7, meshParentStrength__field_descriptor),
                 (Data.ProtoLens.Tag 8, version__field_descriptor),
                 (Data.ProtoLens.Tag 9, uptime__field_descriptor),
                 (Data.ProtoLens.Tag 10, parent__field_descriptor),
                 (Data.ProtoLens.Tag 11, cpuTime__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _RuntimeStats'_unknownFields
              (\ x__ y__ -> x__{_RuntimeStats'_unknownFields = y__})
        defMessage
          = RuntimeStats{_RuntimeStats'minFreeHeap =
                           Data.ProtoLens.fieldDefault,
                         _RuntimeStats'currentFreeHeap = Data.ProtoLens.fieldDefault,
                         _RuntimeStats'cpuUtilization = Data.ProtoLens.fieldDefault,
                         _RuntimeStats'isRoot = Data.ProtoLens.fieldDefault,
                         _RuntimeStats'children = Data.Vector.Generic.empty,
                         _RuntimeStats'wifiStrength = Data.ProtoLens.fieldDefault,
                         _RuntimeStats'meshParentStrength = Data.ProtoLens.fieldDefault,
                         _RuntimeStats'version = Data.ProtoLens.fieldDefault,
                         _RuntimeStats'uptime = Data.ProtoLens.fieldDefault,
                         _RuntimeStats'parent = Prelude.Nothing,
                         _RuntimeStats'cpuTime = Data.ProtoLens.fieldDefault,
                         _RuntimeStats'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     RuntimeStats ->
                       Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                         Data.ProtoLens.Encoding.Growing.RealWorld
                         NodeId
                         -> Data.ProtoLens.Encoding.Bytes.Parser RuntimeStats
                loop x mutable'children
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do frozen'children <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                 (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                    mutable'children)
                            let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'children")
                                    frozen'children
                                    x))
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                8 -> do y <- (Prelude.fmap Prelude.fromIntegral
                                                Data.ProtoLens.Encoding.Bytes.getVarInt)
                                               Data.ProtoLens.Encoding.Bytes.<?> "minFreeHeap"
                                        loop
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"minFreeHeap")
                                             y
                                             x)
                                          mutable'children
                                16 -> do y <- (Prelude.fmap Prelude.fromIntegral
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "currentFreeHeap"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"currentFreeHeap")
                                              y
                                              x)
                                           mutable'children
                                24 -> do y <- (Prelude.fmap Prelude.fromIntegral
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "cpuUtilization"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"cpuUtilization")
                                              y
                                              x)
                                           mutable'children
                                32 -> do y <- (Prelude.fmap ((Prelude./=) 0)
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "isRoot"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"isRoot")
                                              y
                                              x)
                                           mutable'children
                                42 -> do !y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "children"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append
                                                   mutable'children
                                                   y)
                                         loop x v
                                48 -> do y <- (Prelude.fmap Prelude.fromIntegral
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "wifiStrength"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"wifiStrength")
                                              y
                                              x)
                                           mutable'children
                                56 -> do y <- (Prelude.fmap Prelude.fromIntegral
                                                 Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "meshParentStrength"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"meshParentStrength")
                                              y
                                              x)
                                           mutable'children
                                66 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "version"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"version")
                                              y
                                              x)
                                           mutable'children
                                72 -> do y <- (Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "uptime"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"uptime")
                                              y
                                              x)
                                           mutable'children
                                82 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "parent"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"parent")
                                              y
                                              x)
                                           mutable'children
                                88 -> do y <- (Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "cpu_time"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"cpuTime")
                                              y
                                              x)
                                           mutable'children
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
                                             mutable'children
              in
              (do mutable'children <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        Data.ProtoLens.Encoding.Growing.new
                  loop Data.ProtoLens.defMessage mutable'children)
                Data.ProtoLens.Encoding.Bytes.<?> "RuntimeStats"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"minFreeHeap") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 8) Data.Monoid.<>
                      ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                         Prelude.fromIntegral)
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"currentFreeHeap")
                            _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 16) Data.Monoid.<>
                        ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                           Prelude.fromIntegral)
                          _v)
                   Data.Monoid.<>
                   (let _v
                          = Lens.Family2.view (Data.ProtoLens.Field.field @"cpuUtilization")
                              _x
                      in
                      if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                        Data.Monoid.mempty else
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 24) Data.Monoid.<>
                          ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                             Prelude.fromIntegral)
                            _v)
                     Data.Monoid.<>
                     (let _v
                            = Lens.Family2.view (Data.ProtoLens.Field.field @"isRoot") _x
                        in
                        if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty else
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 32) Data.Monoid.<>
                            ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                               (\ b -> if b then 1 else 0))
                              _v)
                       Data.Monoid.<>
                       (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                          (\ _v ->
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 42) Data.Monoid.<>
                               (((\ bs ->
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                  Prelude.. Data.ProtoLens.encodeMessage)
                                 _v)
                          (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'children")
                             _x))
                         Data.Monoid.<>
                         (let _v
                                = Lens.Family2.view (Data.ProtoLens.Field.field @"wifiStrength") _x
                            in
                            if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                              Data.Monoid.mempty else
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 48) Data.Monoid.<>
                                ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                                   Prelude.fromIntegral)
                                  _v)
                           Data.Monoid.<>
                           (let _v
                                  = Lens.Family2.view
                                      (Data.ProtoLens.Field.field @"meshParentStrength")
                                      _x
                              in
                              if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty else
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 56) Data.Monoid.<>
                                  ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                                     Prelude.fromIntegral)
                                    _v)
                             Data.Monoid.<>
                             (let _v
                                    = Lens.Family2.view (Data.ProtoLens.Field.field @"version") _x
                                in
                                if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                                  Data.Monoid.mempty else
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 66) Data.Monoid.<>
                                    (((\ bs ->
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           Data.Monoid.<>
                                           Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                       Prelude.. Data.Text.Encoding.encodeUtf8)
                                      _v)
                               Data.Monoid.<>
                               (let _v
                                      = Lens.Family2.view (Data.ProtoLens.Field.field @"uptime") _x
                                  in
                                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                                    Data.Monoid.mempty else
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 72) Data.Monoid.<>
                                      Data.ProtoLens.Encoding.Bytes.putVarInt _v)
                                 Data.Monoid.<>
                                 (case
                                    Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'parent")
                                      _x
                                    of
                                      (Prelude.Nothing) -> Data.Monoid.mempty
                                      Prelude.Just _v -> (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                            82)
                                                           Data.Monoid.<>
                                                           (((\ bs ->
                                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                   (Prelude.fromIntegral
                                                                      (Data.ByteString.length bs)))
                                                                  Data.Monoid.<>
                                                                  Data.ProtoLens.Encoding.Bytes.putBytes
                                                                    bs))
                                                              Prelude..
                                                              Data.ProtoLens.encodeMessage)
                                                             _v)
                                   Data.Monoid.<>
                                   (let _v
                                          = Lens.Family2.view
                                              (Data.ProtoLens.Field.field @"cpuTime")
                                              _x
                                      in
                                      if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                                        Data.Monoid.mempty else
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 88) Data.Monoid.<>
                                          Data.ProtoLens.Encoding.Bytes.putVarInt _v)
                                     Data.Monoid.<>
                                     Data.ProtoLens.Encoding.Wire.buildFieldSet
                                       (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData RuntimeStats where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_RuntimeStats'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_RuntimeStats'minFreeHeap x__)
                    (Control.DeepSeq.deepseq (_RuntimeStats'currentFreeHeap x__)
                       (Control.DeepSeq.deepseq (_RuntimeStats'cpuUtilization x__)
                          (Control.DeepSeq.deepseq (_RuntimeStats'isRoot x__)
                             (Control.DeepSeq.deepseq (_RuntimeStats'children x__)
                                (Control.DeepSeq.deepseq (_RuntimeStats'wifiStrength x__)
                                   (Control.DeepSeq.deepseq (_RuntimeStats'meshParentStrength x__)
                                      (Control.DeepSeq.deepseq (_RuntimeStats'version x__)
                                         (Control.DeepSeq.deepseq (_RuntimeStats'uptime x__)
                                            (Control.DeepSeq.deepseq (_RuntimeStats'parent x__)
                                               (Control.DeepSeq.deepseq (_RuntimeStats'cpuTime x__)
                                                  (())))))))))))))
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.version' @:: Lens' SetVersion Data.Text.Text@
 -}
data SetVersion = SetVersion{_SetVersion'version ::
                             !Data.Text.Text,
                             _SetVersion'_unknownFields :: !Data.ProtoLens.FieldSet}
                    deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SetVersion where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SetVersion "version"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _SetVersion'version
               (\ x__ y__ -> x__{_SetVersion'version = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message SetVersion where
        messageName _ = Data.Text.pack "SetVersion"
        fieldsByTag
          = let version__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "version"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"version"))
                      :: Data.ProtoLens.FieldDescriptor SetVersion
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, version__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _SetVersion'_unknownFields
              (\ x__ y__ -> x__{_SetVersion'_unknownFields = y__})
        defMessage
          = SetVersion{_SetVersion'version = Data.ProtoLens.fieldDefault,
                       _SetVersion'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     SetVersion -> Data.ProtoLens.Encoding.Bytes.Parser SetVersion
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "version"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"version")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "SetVersion"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"version") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                      (((\ bs ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                             Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Prelude.. Data.Text.Encoding.encodeUtf8)
                        _v)
                 Data.Monoid.<>
                 Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData SetVersion where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_SetVersion'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_SetVersion'version x__) (())))
newtype StreamState'UnrecognizedValue = StreamState'UnrecognizedValue Data.Int.Int32
                                          deriving (Prelude.Eq, Prelude.Ord, Prelude.Show)
data StreamState = Normal
                 | Debiasing
                 | StreamState'Unrecognized !StreamState'UnrecognizedValue
                     deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum StreamState where
        maybeToEnum 0 = Prelude.Just Normal
        maybeToEnum 1 = Prelude.Just Debiasing
        maybeToEnum k
          = Prelude.Just
              (StreamState'Unrecognized
                 (StreamState'UnrecognizedValue (Prelude.fromIntegral k)))
        showEnum Normal = "normal"
        showEnum Debiasing = "debiasing"
        showEnum
          (StreamState'Unrecognized (StreamState'UnrecognizedValue k))
          = Prelude.show k
        readEnum k
          | (k) Prelude.== "normal" = Prelude.Just Normal
          | (k) Prelude.== "debiasing" = Prelude.Just Debiasing
        readEnum k
          = (Text.Read.readMaybe k) Prelude.>>= Data.ProtoLens.maybeToEnum
instance Prelude.Bounded StreamState where
        minBound = Normal
        maxBound = Debiasing
instance Prelude.Enum StreamState where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 (("toEnum: unknown value for enum StreamState: ") Prelude.++
                    Prelude.show k__))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum Normal = 0
        fromEnum Debiasing = 1
        fromEnum
          (StreamState'Unrecognized (StreamState'UnrecognizedValue k))
          = Prelude.fromIntegral k
        succ Debiasing
          = Prelude.error
              "StreamState.succ: bad argument Debiasing. This value would be out of bounds."
        succ Normal = Debiasing
        succ (StreamState'Unrecognized _)
          = Prelude.error
              "StreamState.succ: bad argument: unrecognized value"
        pred Normal
          = Prelude.error
              "StreamState.pred: bad argument Normal. This value would be out of bounds."
        pred Debiasing = Normal
        pred (StreamState'Unrecognized _)
          = Prelude.error
              "StreamState.pred: bad argument: unrecognized value"
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault StreamState where
        fieldDefault = Normal
instance Control.DeepSeq.NFData StreamState where
        rnf x__ = Prelude.seq x__ (())
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.uuid' @:: Lens' Transaction Data.Text.Text@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.start' @:: Lens' Transaction Data.Word.Word64@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.etrs' @:: Lens' Transaction
  (Data.Map.Map Data.Text.Text EnergyTransactionRequest)@
 -}
data Transaction = Transaction{_Transaction'uuid ::
                               !Data.Text.Text,
                               _Transaction'start :: !Data.Word.Word64,
                               _Transaction'etrs ::
                               !(Data.Map.Map Data.Text.Text EnergyTransactionRequest),
                               _Transaction'_unknownFields :: !Data.ProtoLens.FieldSet}
                     deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Transaction where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Transaction "uuid"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Transaction'uuid
               (\ x__ y__ -> x__{_Transaction'uuid = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Transaction "start"
           (Data.Word.Word64)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Transaction'start
               (\ x__ y__ -> x__{_Transaction'start = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Transaction "etrs"
           (Data.Map.Map Data.Text.Text EnergyTransactionRequest)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Transaction'etrs
               (\ x__ y__ -> x__{_Transaction'etrs = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message Transaction where
        messageName _ = Data.Text.pack "Transaction"
        fieldsByTag
          = let uuid__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "uuid"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"uuid"))
                      :: Data.ProtoLens.FieldDescriptor Transaction
                start__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "start"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"start"))
                      :: Data.ProtoLens.FieldDescriptor Transaction
                etrs__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "etrs"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Transaction'EtrsEntry)
                      (Data.ProtoLens.MapField (Data.ProtoLens.Field.field @"key")
                         (Data.ProtoLens.Field.field @"value")
                         (Data.ProtoLens.Field.field @"etrs"))
                      :: Data.ProtoLens.FieldDescriptor Transaction
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, uuid__field_descriptor),
                 (Data.ProtoLens.Tag 2, start__field_descriptor),
                 (Data.ProtoLens.Tag 3, etrs__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Transaction'_unknownFields
              (\ x__ y__ -> x__{_Transaction'_unknownFields = y__})
        defMessage
          = Transaction{_Transaction'uuid = Data.ProtoLens.fieldDefault,
                        _Transaction'start = Data.ProtoLens.fieldDefault,
                        _Transaction'etrs = Data.Map.empty,
                        _Transaction'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     Transaction -> Data.ProtoLens.Encoding.Bytes.Parser Transaction
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "uuid"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"uuid") y
                                              x)
                                16 -> do y <- (Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                Data.ProtoLens.Encoding.Bytes.<?> "start"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"start") y
                                              x)
                                26 -> do !(entry ::
                                             Transaction'EtrsEntry) <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                           Data.ProtoLens.Encoding.Bytes.isolate
                                                                             (Prelude.fromIntegral
                                                                                len)
                                                                             Data.ProtoLens.parseMessage)
                                                                         Data.ProtoLens.Encoding.Bytes.<?>
                                                                         "etrs"
                                         let key
                                               = Lens.Family2.view
                                                   (Data.ProtoLens.Field.field @"key")
                                                   entry
                                             value
                                               = Lens.Family2.view
                                                   (Data.ProtoLens.Field.field @"value")
                                                   entry
                                           in
                                           loop
                                             (Lens.Family2.over (Data.ProtoLens.Field.field @"etrs")
                                                (\ !t -> Data.Map.insert key value t)
                                                x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "Transaction"
        buildMessage
          = (\ _x ->
               (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"uuid") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                      (((\ bs ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                             Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Prelude.. Data.Text.Encoding.encodeUtf8)
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"start") _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 16) Data.Monoid.<>
                        Data.ProtoLens.Encoding.Bytes.putVarInt _v)
                   Data.Monoid.<>
                   (Data.Monoid.mconcat
                      (Prelude.map
                         (\ _v ->
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26) Data.Monoid.<>
                              (((\ bs ->
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Prelude.. Data.ProtoLens.encodeMessage)
                                (Lens.Family2.set (Data.ProtoLens.Field.field @"key")
                                   (Prelude.fst _v)
                                   (Lens.Family2.set (Data.ProtoLens.Field.field @"value")
                                      (Prelude.snd _v)
                                      (Data.ProtoLens.defMessage :: Transaction'EtrsEntry))))
                         (Data.Map.toList
                            (Lens.Family2.view (Data.ProtoLens.Field.field @"etrs") _x))))
                     Data.Monoid.<>
                     Data.ProtoLens.Encoding.Wire.buildFieldSet
                       (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Transaction where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_Transaction'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_Transaction'uuid x__)
                    (Control.DeepSeq.deepseq (_Transaction'start x__)
                       (Control.DeepSeq.deepseq (_Transaction'etrs x__) (())))))
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.key' @:: Lens' Transaction'EtrsEntry Data.Text.Text@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.value' @:: Lens' Transaction'EtrsEntry EnergyTransactionRequest@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.maybe'value' @:: Lens' Transaction'EtrsEntry
  (Prelude.Maybe EnergyTransactionRequest)@
 -}
data Transaction'EtrsEntry = Transaction'EtrsEntry{_Transaction'EtrsEntry'key
                                                   :: !Data.Text.Text,
                                                   _Transaction'EtrsEntry'value ::
                                                   !(Prelude.Maybe EnergyTransactionRequest),
                                                   _Transaction'EtrsEntry'_unknownFields ::
                                                   !Data.ProtoLens.FieldSet}
                               deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Transaction'EtrsEntry where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Transaction'EtrsEntry "key"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Transaction'EtrsEntry'key
               (\ x__ y__ -> x__{_Transaction'EtrsEntry'key = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Transaction'EtrsEntry
           "value"
           (EnergyTransactionRequest)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Transaction'EtrsEntry'value
               (\ x__ y__ -> x__{_Transaction'EtrsEntry'value = y__}))
              Prelude.. Data.ProtoLens.maybeLens Data.ProtoLens.defMessage
instance Data.ProtoLens.Field.HasField Transaction'EtrsEntry
           "maybe'value"
           (Prelude.Maybe EnergyTransactionRequest)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Transaction'EtrsEntry'value
               (\ x__ y__ -> x__{_Transaction'EtrsEntry'value = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message Transaction'EtrsEntry where
        messageName _ = Data.Text.pack "Transaction.EtrsEntry"
        fieldsByTag
          = let key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"key"))
                      :: Data.ProtoLens.FieldDescriptor Transaction'EtrsEntry
                value__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "value"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor EnergyTransactionRequest)
                      (Data.ProtoLens.OptionalField
                         (Data.ProtoLens.Field.field @"maybe'value"))
                      :: Data.ProtoLens.FieldDescriptor Transaction'EtrsEntry
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, key__field_descriptor),
                 (Data.ProtoLens.Tag 2, value__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Transaction'EtrsEntry'_unknownFields
              (\ x__ y__ -> x__{_Transaction'EtrsEntry'_unknownFields = y__})
        defMessage
          = Transaction'EtrsEntry{_Transaction'EtrsEntry'key =
                                    Data.ProtoLens.fieldDefault,
                                  _Transaction'EtrsEntry'value = Prelude.Nothing,
                                  _Transaction'EtrsEntry'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     Transaction'EtrsEntry ->
                       Data.ProtoLens.Encoding.Bytes.Parser Transaction'EtrsEntry
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "key"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y
                                              x)
                                18 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.isolate
                                                    (Prelude.fromIntegral len)
                                                    Data.ProtoLens.parseMessage)
                                                Data.ProtoLens.Encoding.Bytes.<?> "value"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "EtrsEntry"
        buildMessage
          = (\ _x ->
               (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                      (((\ bs ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                             Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Prelude.. Data.Text.Encoding.encodeUtf8)
                        _v)
                 Data.Monoid.<>
                 (case
                    Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'value") _x of
                      (Prelude.Nothing) -> Data.Monoid.mempty
                      Prelude.Just _v -> (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                                           Data.Monoid.<>
                                           (((\ bs ->
                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                   (Prelude.fromIntegral
                                                      (Data.ByteString.length bs)))
                                                  Data.Monoid.<>
                                                  Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                              Prelude.. Data.ProtoLens.encodeMessage)
                                             _v)
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Transaction'EtrsEntry where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_Transaction'EtrsEntry'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_Transaction'EtrsEntry'key x__)
                    (Control.DeepSeq.deepseq (_Transaction'EtrsEntry'value x__) (()))))
{- | Fields :

    * 'Proto.NodeMessageSchema.NodeMessages_Fields.updateStatus' @:: Lens' UpdateStatus Data.Word.Word32@
    * 'Proto.NodeMessageSchema.NodeMessages_Fields.timeOfLastUpdate' @:: Lens' UpdateStatus Data.Text.Text@
 -}
data UpdateStatus = UpdateStatus{_UpdateStatus'updateStatus ::
                                 !Data.Word.Word32,
                                 _UpdateStatus'timeOfLastUpdate :: !Data.Text.Text,
                                 _UpdateStatus'_unknownFields :: !Data.ProtoLens.FieldSet}
                      deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show UpdateStatus where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField UpdateStatus "updateStatus"
           (Data.Word.Word32)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _UpdateStatus'updateStatus
               (\ x__ y__ -> x__{_UpdateStatus'updateStatus = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField UpdateStatus
           "timeOfLastUpdate"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _UpdateStatus'timeOfLastUpdate
               (\ x__ y__ -> x__{_UpdateStatus'timeOfLastUpdate = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message UpdateStatus where
        messageName _ = Data.Text.pack "UpdateStatus"
        fieldsByTag
          = let updateStatus__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "update_status"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"updateStatus"))
                      :: Data.ProtoLens.FieldDescriptor UpdateStatus
                timeOfLastUpdate__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "time_of_last_update"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"timeOfLastUpdate"))
                      :: Data.ProtoLens.FieldDescriptor UpdateStatus
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, updateStatus__field_descriptor),
                 (Data.ProtoLens.Tag 2, timeOfLastUpdate__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _UpdateStatus'_unknownFields
              (\ x__ y__ -> x__{_UpdateStatus'_unknownFields = y__})
        defMessage
          = UpdateStatus{_UpdateStatus'updateStatus =
                           Data.ProtoLens.fieldDefault,
                         _UpdateStatus'timeOfLastUpdate = Data.ProtoLens.fieldDefault,
                         _UpdateStatus'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     UpdateStatus -> Data.ProtoLens.Encoding.Bytes.Parser UpdateStatus
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                8 -> do y <- (Prelude.fmap Prelude.fromIntegral
                                                Data.ProtoLens.Encoding.Bytes.getVarInt)
                                               Data.ProtoLens.Encoding.Bytes.<?> "update_status"
                                        loop
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"updateStatus")
                                             y
                                             x)
                                18 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?>
                                                "time_of_last_update"
                                         loop
                                           (Lens.Family2.set
                                              (Data.ProtoLens.Field.field @"timeOfLastUpdate")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "UpdateStatus"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"updateStatus") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 8) Data.Monoid.<>
                      ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                         Prelude.fromIntegral)
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"timeOfLastUpdate")
                            _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 18) Data.Monoid.<>
                        (((\ bs ->
                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                (Prelude.fromIntegral (Data.ByteString.length bs)))
                               Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Prelude.. Data.Text.Encoding.encodeUtf8)
                          _v)
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData UpdateStatus where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_UpdateStatus'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_UpdateStatus'updateStatus x__)
                    (Control.DeepSeq.deepseq (_UpdateStatus'timeOfLastUpdate x__)
                       (()))))