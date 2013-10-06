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

import Data.List
import Data.Monoid
import Solar.Data.KV.Utilities()

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

instance (Ord n, Ord r, Ord c, Monoid n) => Monoid (KVMeta n r c) where
    mempty = KVMeta mempty [] [] mempty mempty mempty False
    mappend (KVMeta i c r lm lmc ca v) (KVMeta i' c' r' lm' lmc' ca' v') =
        KVMeta i'' c'' r'' lm'' lmc'' ca'' v''
        where
            i''     = mappend i i'
            c''     = nub.sort $ union c c'
            r''     = nub.sort $ union r r'
            lm''    = mappend lm lm'
            lmc''   = mappend lmc lmc'
            ca''    = mappend ca ca'
            v''     = or [v, v']
