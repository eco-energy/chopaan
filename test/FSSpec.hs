module FSSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck

import Chopaan.Kibbutz.FS
import Data.IORef

-- data Command r
--   = Create
--   | Read (Reference (Opaque (IORef Int)) r)
--   | Write (Reference (Opaque (IORef Int)) r) Int
--   | Increment (Reference (Opaque (IORef Int)) r)

-- data Response r
--   = Created (Reference (Opaque (IORef Int)) r)
--   | ReadValue Int
--   | Written
--   | Incremented

-- data Bug = None | Logic | Race
--   deriving Eq

-- semantics :: Bug -> Command Concrete -> IO (Response Concrete)
-- semantics bug cmd = case cmd of
--   Create -> Created <$> (reference . Opaque <$> newIORef 0)
--   Read ref -> ReadValue <$> readIORef (opaque ref)
--   Write ref i -> Written <@ writeIORef (opaque ref) i'
--     where
--       i' | bug == Logi && i `elem` [5..10] = i + 1
--          | otherwise = i
--   Increment ref -> do
--     if bug == Race
--     then do
--       i <- readIOref (opaque ref)
--       threadDelay =<< randomRIO (0, 5000)
--       writeIORef (opaque ref) (i + 1)
--     else
--       atomicModifyIORef' (opaque ref) (\i -> (i + 1, ()))
--     return Incremented
