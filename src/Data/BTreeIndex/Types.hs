{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving, DeriveAnyClass #-}
module Data.BTreeIndex.Types where

-- $ Stolen from Ben Gamari.
-- $ https://hackage.haskell.org/package/b-tree-0.1.4/

import GHC.Generics
import Control.Monad (when)
import Data.Int
import Data.Word
import Data.Typeable
import Prelude

import Codec.Winery


import qualified Data.Vector as V

import Control.Lens
import qualified Data.ByteString as BS

-- | An offset within the stream
type Offset = Int64

-- | The number of entries in a B-tree
type Size = Word64

-- | The maximum number of children of a B-tree inner node
type Order = Word64

-- | @'OnDisk' a@ is a reference to an object of type @a@ on disk.
-- The offset does not include the header; e.g. the first object after
-- the header is located at offset 0.
newtype OnDisk a = OnDisk Offset
                 deriving (Show, Eq, Ord)
                 deriving newtype Serialise --via (Serialise Offset)

-- | A tree leaf (e.g. key/value pair)
data BLeaf k e = BLeaf !k !e
               deriving (Generic, Functor)
               deriving (Serialise) via (WineryProduct (BLeaf k e))

deriving instance (Show k, Show e) => Show (BLeaf k e)

-- | This only compares on the keys
instance (Eq k) => Eq (BLeaf k e) where
    BLeaf a _ == BLeaf b _ = a == b

-- | This only compares on the keys
instance Ord k => Ord (BLeaf k e) where
    compare (BLeaf a _) (BLeaf b _) = compare a b
    {-# INLINE compare #-}


-- | @'BTree' k f e@ is a B* tree of key type @k@ with elements of type @e@.
-- Subtree references are contained within a type @f@.
--
-- The 'Node' constructor contains a left child, and a list of key/child pairs
-- where each child's keys are greater than or equal to the given key.
data BTree k f e = Node (f (BTree k f e)) (V.Vector (k, f (BTree k f e)))
                 | Leaf !(BLeaf k e)
                 deriving (Generic)
                 
deriving instance (Show e, Show k, Show (f (BTree k f e))) => Show (BTree k f e)
deriving instance (Eq e, Eq k, Eq (f (BTree k f e))) => Eq (BTree k f e)

deriving via
  (WineryVariant (BTree k f e)) instance
  (Typeable k, Typeable f, Typeable e
  , Serialise k, Serialise e, Serialise (f (BTree k f e))) => Serialise (BTree k f e) 

magic :: Word64
magic = 0xdeadbeefbbbbcccc

-- | B-tree file header
data BTreeHeader k e = BTreeHeader { _btMagic   :: !Word64
                                   , _btVersion :: !Word64
                                   , _btOrder   :: !Order
                                   , _btSize    :: !Size
                                   , _btRoot    :: !(Maybe (OnDisk (BTree k OnDisk e)))
                                     -- ^ 'Nothing' represents an empty tree
                                   }
                 deriving (Show, Eq, Generic)
                 deriving (Serialise) via (WineryProduct (BTreeHeader k e))
makeLenses ''BTreeHeader

-- | It is critical that this encoding is of fixed size -- I Will check that assumption!
--  instance (Typeable k, Typeable e) => Serialise (BTreeHeader k e) -- where
    -- get = do
    --     _btMagic <- get
    --     _btVersion <- get
    --     _btOrder <- get
    --     _btSize <- get
    --     root <- get
    --     let _btRoot = if root == OnDisk 0 then Nothing else Just root
    --     return BTreeHeader {..}
    -- put (BTreeHeader {..}) = do
    --     put _btMagic
    --     put _btVersion
    --     put _btOrder
    --     put _btSize
    --     put $ fromMaybe (OnDisk 0) _btRoot

validateHeader :: BTreeHeader k e -> Either String ()
validateHeader hdr = do
    when (hdr^.btMagic /= magic) $ Left "Invalid magic number"
    when (hdr^.btVersion > 1) $ Left "Invalid version"

-- | A read-only B-tree for lookups
data LookupTree k e = LookupTree { _ltData    :: !BS.ByteString
                                 , _ltHeader  :: !(BTreeHeader k e)
                                 }
makeLenses ''LookupTree
