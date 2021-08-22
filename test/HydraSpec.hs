{-# LANGUAGE PackageImports #-}
module HydraSpec where

import qualified Streamly.Prelude as S
import Streamly.Internal.Data.Time.Units (MilliSecond64(..))
import Test.Hspec
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Instances.Time
import Test.QuickCheck.Instances.Text

import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Chopaan.Comm.S3
import Streamly.Binary
import Streamly.Internal.Data.Array.Foreign as A

spec :: Spec
spec = describe "S3 Hydration Checks" $ do
  it "length corresponds to time units" $ do
    sec' <- S.length xs
    -- m' <- S.length ms
    -- h' <- S.length hs
    -- d' <- S.length ds
    -- w' <- S.length ws
    sec' `shouldBe` 8641
    -- m' `shouldBe` 1440
    -- h' `shouldBe` 24
    -- d' `shouldBe` 1
    -- w' `shouldBe` 1
  it "prefix length is sane" $ do
    sec' <- S.the $ fmap T.length xs
    m' <- S.the $ fmap T.length ms
    h' <- S.the $ fmap T.length hs
    d' <- S.the $ fmap T.length ds
    w' <- S.the $ fmap T.length ws
    sec' `shouldBe` (Just 9)
    m' `shouldBe` (Just 7)
    h' `shouldBe` (Just 5) 
    w' `shouldBe` (Just 3)
  --it "length prefix encoding decoding" $ property prop_enc_dec_roundtrip

arbS :: Int -> SerialT IO T.Text
arbS n = S.fromListM (arbs n)

-- prop_enc_dec_roundtrip :: Property
-- prop_enc_dec_roundtrip = forAll (arbitrary @T.Text) (\t ->
--                                                        let s = S.repeat t
--                                                        in S.all ((==)) 
--                                                        )


unM (MilliSecond64 w') = w'
w = unM worldStart
s = posixSecondsToUTCTime . fromIntegral $ (div w  1000)
e = posixSecondsToUTCTime . fromIntegral $ (div w  1000) + 86400
xs = prefixRange Second s e
ms = prefixRange Minute s e
hs = prefixRange Hour s e
ds = prefixRange Day s e
ws = prefixRange Week s e

--x' <- S.length xs
