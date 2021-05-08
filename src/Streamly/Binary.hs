module Streamly.Binary
  ( decodeStream,
    decodeStreamGet,
    encodeStream,
    encodeStreamPut,
  )
where

import Control.Monad.Fail (MonadFail)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Streamly.Internal.Data.Stream.StreamD as D

import Streamly (SerialT)
import Streamly.Internal.Data.Pipe.Types
import Streamly.Internal.Data.Pipe
import qualified Streamly.Prelude as S

{-# INLINE transform #-}
transform :: (S.IsStream t, Monad m) => Pipe m a b -> t m a -> t m b
transform pipe xs = D.fromStreamD $ D.transform pipe (D.toStreamD xs)

-- | Decode stream of bytestrings given that there exists instance of 'Binary'
-- for target type. Bytestrings do not have to be aligned in any way.
decodeStream :: (Binary a, MonadFail m) => SerialT m BS.ByteString -> SerialT m a
decodeStream = decodeStreamGet get

-- | Decode stream of bytestrings using 'Get' from 'Binary'.
-- Bytestrings do not have to be aligned in any way.
decodeStreamGet :: MonadFail m => Get a -> SerialT m BS.ByteString -> SerialT m a
decodeStreamGet g = transform $ Pipe consume (produce g) (runGetIncremental g)

-- | Encode stream of elements to bytestrings given that there exists instance of 'Binary'
-- for source type. Resulting bytestrings are not guaranteed to be aligned in any way.
encodeStream :: (Binary a, MonadFail m) => SerialT m a -> SerialT m BS.ByteString
encodeStream = encodeStreamPut put

-- | Encode stream of elements using 'Put' from 'Binary'.
-- Resulting bytestrings are not guaranteed to be aligned in any way.
encodeStreamPut :: (MonadFail m) => (a -> Put) -> SerialT m a -> SerialT m BS.ByteString
encodeStreamPut p = S.concatMap (S.fromList . BL.toChunks) . S.map (runPut . p)

consume :: MonadFail m => Decoder a -> BS.ByteString -> m (Step (PipeState (Decoder a) (Decoder a)) a)
consume d@Done {} input = return $ Continue (Produce $ pushChunk d input)
consume (Partial f) input =
  if BS.null input
    then return (Continue (Consume (f Nothing)))
    else return (Continue (Produce (f (Just input))))
consume (Fail _ _ msg) _ = fail msg

produce :: MonadFail m => Get a -> Decoder a -> m (Step (PipeState (Decoder a) (Decoder a)) a)
produce g (Done unused _ output) =
  if BS.null unused
    then return $ Yield output (Consume (runGetIncremental g))
    else return $ Yield output (Produce (runGetIncremental g `pushChunk` unused))
produce _ d@(Partial _) = return $ Continue (Consume d)
produce _ (Fail _ _ msg) = fail msg
