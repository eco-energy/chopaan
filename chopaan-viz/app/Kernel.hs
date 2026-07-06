{-# LANGUAGE ForeignFunctionInterface #-}

-- | FFI binding to the categorified Hopfield kernel (@cbits/hopfield_step.c@).
--
-- That C was produced by Categorifier lowering chopaan's Hopfield step
-- (@F.hs@ / Manin-Marcolli Eq. 6.2) — i.e. it *is* the Haskell exported to C.
-- The renderer calls it directly so the on-screen physics is byte-for-byte the
-- same morphism the PufferLib RL env settles.
--
-- Signature (see hopfield_step.h): 11 typed input arrays, then 11 typed output
-- arrays. Only @input_double[30]@ (#11) and @output_double[7]@ (#22) are used;
-- the rest are zero-length — we pass NULL for them.
module Kernel
  ( settle
  , Flows(..)
  ) where

import           Foreign.Marshal.Array (withArray, allocaArray, peekArray)
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.C.Types (CDouble)

foreign import ccall unsafe "hopfield_step"
  c_hopfield_step
    :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble
    -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble
    -> Ptr CDouble                              -- #11  input_double[30]
    -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble
    -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble
    -> Ptr CDouble                              -- #22  output_double[7]
    -> IO ()

-- | Settled edge flows (P,Q for e0,e1,e2) plus net grid import at the slack.
data Flows = Flows
  { fEdge   :: [(Double,Double)]   -- ^ length 3
  , fImport :: Double
  } deriving (Show)

-- | Run the kernel. @xs@ must be exactly the 30 @input_double@ values in
-- @F.hs@ field order:
--   b00 b01 b02  b10 b11 b12  b20 b21 b22  b30 b31 b32   (4×3 incidence)
--   ip0..ip3  iq0..iq3  g0 g1 g2  x0p x0q x1p x1q x2p x2q  alpha
settle :: [Double] -> IO Flows
settle xs =
  withArray (map realToFrac xs) $ \inp ->
  allocaArray 7 $ \out -> do
    let z = nullPtr
    c_hopfield_step z z z z z z z z z z inp z z z z z z z z z z out
    o <- map realToFrac <$> peekArray 7 out
    pure Flows { fEdge   = [(o!!0,o!!1),(o!!2,o!!3),(o!!4,o!!5)]
               , fImport = o!!6 }
