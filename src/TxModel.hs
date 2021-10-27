module TxModel where

import Data.SBV
import Data.SBV.Tools.CodeGen

gridVoltageMax :: SFloat
gridVoltageMax = 54

type BV = SFloat

type GV = SFloat
type Current = SFloat

type TxPw = SFloat
type Pw = SFloat

type BoostDC = SFloat
type BuckDC = SFloat

targetPower :: TxPw
targetPower = undefined

pw :: BV -> Current -> Pw 
pw = (*)

buckDCMax , buckDCMin, boostDCMax, boostDCMin :: SFloat

buckDCMin = 290
buckDCMax = 1500

boostDCMin = 900
boostDCMax = 1490

dcDelta :: SFloat
dcDelta = 1

window :: SFloat -> SFloat -> SFloat -> SBool
window w tar meas = (meas .> tar - w) .&& (meas .< tar + w) 

boost :: TxPw -> GV -> Current -> BoostDC -> BoostDC
boost target gv c cDC' = clamp boostDCMin boostDCMax
  (ite (pwr .<= tx - 1)
    (cDC + dcDelta)
    (ite (pwr .> tx + 1)
      (cDC - dcDelta)
      cDC))
  where
    cDC = clamp boostDCMin boostDCMax cDC'
    tx = clamp 0 500 target
    pwr = pw (clamp gvMin gvMax gv) (clamp cMax cMin c)
    clamp :: SFloat -> SFloat -> SFloat -> SFloat
    clamp min' max' val = smax min' $ smin max' val

buck :: TxPw -> GV -> Current -> BuckDC -> BuckDC
buck target gv c cDC' = clamp buckDCMin buckDCMax
  (ite (pwr .<= tx - 1)
    (cDC + dcDelta)
    (ite (pwr .> tx + 1)
      (cDC - dcDelta)
      cDC))
  where
    cDC = clamp buckDCMin buckDCMax cDC'
    tx = clamp 0 500 target
    pwr = pw (clamp gvMin gvMax gv) (clamp cMax cMin c)
    clamp :: SFloat -> SFloat -> SFloat -> SFloat
    clamp min' max' val = smax min' $ smin max' val


gvMax, gvMin, cMax, cMin, txMin, txMax :: SFloat
gvMin = 0
gvMax = 55
cMin = 0
cMax = 30
txMin = 0
txMax = 0
{--
f :: BV -> Current -> TxPw -> (BoostDC, BuckDC) -> (BoostDC, BuckDC)
f bv current (boostDCP, buckDCP) =
  ite undefined-- (range .> 200 .|| manual .|| timeSince .> maxTimeSince - 2)
  0
  (boostDCP, buckDCP)
--}

p1, p2, p1', p2' :: Predicate
p1 = forAll ["tx", "gv", "c", "dc"] $ \tx gv c dc -> boost tx gv c dc .<= boostDCMax

p2 = forAll ["tx", "gv", "c", "dc"] $ \tx gv c dc ->
  boost tx gv c dc .>= boostDCMin

p1' = forAll ["tx", "gv", "c", "dc"] $ \tx gv c dc -> buck tx gv c dc .<= buckDCMax

p2' = forAll ["tx", "gv", "c", "dc"] $ \tx gv c dc ->
  buck tx gv c dc .>= buckDCMin

  

--3 = forAll ["r", "m", "t"] $ \r m t -> m .=> f r m t .== 0

  
spec = forAll ["tx", "gv", "c", "dc"] $ \tx gv c dc -> 
  let minBoDC = boost tx gv c dc .<= boostDCMax
      maxBoDC = boost tx gv c dc .>= boostDCMin
      minBuDC = buck tx gv c dc .<= buckDCMax
      maxBuDC = buck tx gv c dc .>= buckDCMin
   in minBoDC .&& maxBoDC .&& minBuDC .&& maxBuDC


main = compileToC (Just "cbits") "boost" $
  do cgGenerateDriver False
     cgGenerateMakefile False
     tx <- cgInput "tx"
     gv <- cgInput "gridVoltage"
     c <- cgInput "current"
     dc <- cgInput "boostDC"
     cgReturn $ boost tx gv c dc
