{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Solar.Data.KV.Meta where

import GHC.Generics as G
import Data.Generics as D

import Solar.Data.KV.Identifier
import Solar.Data.KV.Link
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format()

data KVMeta namespace relations classes = KVMeta
    { identifier            :: !(KVIdentifier namespace)
    -- ^ Identifier for this entity
    , classes               :: ![classes]
    -- ^ The classifications for this entity
    , relations             :: ![KVLink namespace relations classes]
    -- ^ Links that this entity has with others
    , lastModified          :: !UTCTime
    -- ^ Modification date for the entire data element
    , lastModifiedContent   :: !UTCTime
    -- ^ Content Modification Date
    , createdAt             :: !UTCTime
    -- ^ When this entity was created
    , invalid               :: !Bool
    -- ^ Invalidation flag
    } deriving (Show, Read, Typeable, Data, G.Generic, Eq, Ord)