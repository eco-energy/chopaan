{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLabels, NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Chopaan.Ui.Base where

import GHC.Generics hiding (R)

import Data.Aeson

import Control.DeepSeq
import Control.Lens hiding (simple, elements)
import Data.Generics.Product
import Data.Generics.Labels

import Linear.Vector
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Linear.Metric
import Linear.Projection


type R = Double

type V3R = V3 R 

type QuatR = Quaternion R

type M44R = M44 R

deriving instance (ToJSON a) => ToJSON (V2 a)
deriving instance (FromJSON a) => FromJSON (V2 a)

deriving instance (ToJSON a) => ToJSON (V3 a)
deriving instance (FromJSON a) => FromJSON (V3 a)

deriving instance (ToJSON a) => ToJSON (V4 a)
deriving instance (FromJSON a) => FromJSON (V4 a)

deriving instance (ToJSON a) => ToJSON (Quaternion a)
deriving instance (FromJSON a) => FromJSON (Quaternion a)


data Obj = Obj
  { _pos :: V3R
  , _rot :: QuatR
  , _scale :: V3R
  , _localTransform :: M44R
  , _worldTransform :: M44R
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)


transformObj :: M44R -> Obj -> Obj
transformObj t o = o
                   & #_pos %~ ((t ^. translation) ^+^) 
                   & (#_localTransform) %~ (t !*!)
                   & (#_worldTransform) %~ (t !*!)
                   

translateObj :: V3R -> Obj -> Obj
translateObj t o = o
                   & #_pos .~ newP
                   & (#_localTransform . translation) .~ newP
                   & (#_worldTransform . translation) .~ newP
  where
    newP = t ^+^ (_pos o)

rotateObj :: QuatR -> Obj -> Obj
rotateObj q o = o
                & #_rot .~ newQ
                & (#_localTransform . _m33) %~ newQT
                & (#_worldTransform . _m33) %~ newQT
  where
    newQ = q * (_rot o)
    newQT p = (fromQuaternion q !*! p)

scaleObj :: V3R -> Obj -> Obj
scaleObj s o = o
               & #_scale .~ s
               & (#_localTransform . _m33) %~ newS
               & (#_worldTransform . _m33) %~ newS
  where
    newS p = (scaled s !*! p)


transformMat :: V3R -> QuatR -> V3R -> M44R
transformMat p r s = mkTransformationMat ((scaled s) !*! (fromQuaternion r)) p

asT :: Obj -> M44R
asT Obj{_pos, _rot, _scale} = transformMat _pos _rot _scale

mkObj :: V3R -> QuatR -> V3R -> Obj
mkObj pos rot scale = Obj pos rot scale locT locT
  where
    locT = transformMat pos rot scale

defQuat :: QuatR
defQuat = axisAngle (V3 0 1 0) 90 

zeroObj :: Obj
zeroObj = mkObj zero zero (V3 1 1 1)
