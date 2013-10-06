{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
module Solar.Data.KV.Link where

import GHC.Generics as G
import Data.Generics as D

import Data.Time.Format()
import Data.Time.Clock (UTCTime(..))
import Data.Monoid
import Data.List

import Solar.Data.KV.Identifier
import Solar.Data.KV.Utilities()
import Solar.Data.Graph.Direction

data KVLink n r c = KVLink
    { linkIdentifier :: !(KVIdentifier n)
    -- ^ Identity of the remote entity
    , linkRelations  :: ![r]
    -- ^ Relation Enums that describe what it means
    , linkClasses    :: ![c]
    -- ^ Class Enums that describe what the entity is
    , linkDirection  :: !KVDirection
    -- ^ Stores the direction of this entity to the remote entity.
    , linkAdded      :: !UTCTime
    -- ^ When this link was added
    , linkInvalid    :: !Bool
    -- ^ Invalidation flag
    } deriving (Show, Read, Typeable, Data, G.Generic, Ord)

instance (Monoid n, Ord r, Ord c) => Monoid (KVLink n r c) where
    mempty = KVLink mempty [] [] mempty mempty False
    mappend (KVLink i r c d a v) (KVLink i' r' c' d' a' v') =
        KVLink i'' r'' c'' d'' a'' v''
        where
            i'' = mappend i i'
            r'' = nub.sort $ union r r'
            c'' = nub.sort $ union c c'
            d'' = mappend d d'
            a'' = mappend a a'
            v'' = or [v, v']
instance (Ord n, Ord r, Ord c) => Eq (KVLink n r c) where
    a == b = and [i == i, r == r', c == c']
        where
            i = linkIdentifier a
            i' = linkIdentifier b
            r = nub.sort $ linkRelations a
            r' = nub.sort $ linkRelations b
            c = nub.sort $ linkClasses a
            c' = nub.sort $ linkClasses b