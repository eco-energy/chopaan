module Chopaan.Machine where

import ConCat.Synchronous
import qualified Streamly.Internal.Data.Unfold as UF


machine :: Mealy k a b -> UF.Unfold m a b
machine = undefined
