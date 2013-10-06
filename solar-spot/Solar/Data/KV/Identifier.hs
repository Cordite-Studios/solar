{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
module Solar.Data.KV.Identifier where

import Data.Text
import GHC.Generics as G
import Data.Typeable
import Data.Generics as D

data KVIdentifier n = KVIdentifier
    { namespace :: !n
    -- ^ Namespace enumeration for where this belongs
    , key       :: !Text
    -- ^ Textual name of the key that can be looked up
    } deriving (Show, Read, Typeable, Data, G.Generic, Eq, Ord)

newtype TaggedIdentifier n r c d c' = TaggedIdentifier
    { untagIdentifier :: KVIdentifier n
    }