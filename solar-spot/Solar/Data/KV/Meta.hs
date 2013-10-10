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

import qualified Data.Set as S
import Data.Monoid
import Solar.Utility.Date()

data KVMeta n r c = KVMeta
    { identifier            :: !(KVIdentifier n)
    -- ^ Identifier for this entity
    , classes               :: !(S.Set c)
    -- ^ The classifications for this entity
    , relations             :: !(S.Set (KVLink n r c))
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
    mempty = KVMeta mempty mempty mempty mempty mempty mempty False
    mappend (KVMeta i c r lm lmc ca v) (KVMeta i' c' r' lm' lmc' ca' v') =
        KVMeta i'' c'' r'' lm'' lmc'' ca'' v''
        where
            i''     = mappend i i'
            c''     = mappend c c'
            r''     = mappend r r'
            lm''    = mappend lm lm'
            lmc''   = mappend lmc lmc'
            ca''    = mappend ca ca'
            v''     = or [v, v']
