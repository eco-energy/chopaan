{-# LANGUAGE TypeOperators #-}
module Chopaan.Node.Ui where


import Control.Applicative
import ConCat.Misc
import ConCat.Graphics.Image
import ConCat.Graphics.Color


import Chopaan.Node.NodeSensors
--import Chopaan.Node.HW
import Chopaan.Node.Components

import DearImGui

toPImageC' :: ToColor c => (d -> a -> p -> c) -> (d -> a -> p -> Color)
toPImageC' = (toPImageC .)

toPImageC'' :: ToColor c => (k -> d -> a -> p -> c) -> (k -> d -> a -> p -> Color)
toPImageC'' = (toPImageC' .)
--toPImageC'' = (fmap toPImageC' .)

--nodeUi :: StoreC -> GenC -> LoadC -> GridC -> R2 -> ImageC
nodeUi :: (StoreC :* GenC :* LoadC :* GridC) -> (R2 -> Color)
nodeUi = toPImageC (uncurry . uncurry . uncurry $ nodeDisks)

nodeEnergy :: StoreC -> ImageC
nodeEnergy = undefined

ndisks :: [R] -> Region
ndisks = foldl (\x y -> x `diffR` disk y) nothing

nodeDisks :: StoreC -> GenC -> LoadC -> GridC -> Region
nodeDisks (StoreC s) (GenC gn) (LoadC l) (GridC gr) = disk gr
  `diffR` disk gn
  `diffR` disk l
  `diffR` disk s


yellow :: Color
yellow = rgb 253 216 53

newtype GenC = GenC R

newtype StoreC = StoreC R

newtype LoadC = LoadC R

newtype GridC = GridC R

instance ToColor GenC where
  toColor = const yellow
  
instance ToColor StoreC where
  toColor = const green

instance ToColor LoadC where
  toColor = const red

instance ToColor GridC where
  toColor (GridC r) = if r > 0 then red else yellow 


raster :: ImageC -> m ()
raster = undefined
