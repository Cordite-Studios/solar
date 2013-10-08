{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
module Solar.Data.KV.Link where

import GHC.Generics as G
import Data.Generics as D

import Data.Time.Format()
import Data.Time.Clock (UTCTime(..))
import Data.Monoid
import qualified Data.Set as S

import Solar.Data.KV.Identifier
import Solar.Data.KV.Utilities()
import Solar.Data.Graph.Direction

data KVLink n r c = KVLink
    { linkIdentifier :: !(KVIdentifier n)
    -- ^ Identity of the remote entity
    , linkRelations  :: !(S.Set r)
    -- ^ Relation Enums that describe what it means
    , linkClasses    :: !(S.Set c)
    -- ^ Class Enums that describe what the entity is
    , linkDirection  :: !KVDirection
    -- ^ Stores the direction of this entity to the remote entity.
    , linkAdded      :: !UTCTime
    -- ^ When this link was added
    , linkInvalid    :: !Bool
    -- ^ Invalidation flag
    } deriving (Show, Read, Typeable, Data, G.Generic, Ord)

instance (Monoid n, Ord r, Ord c) => Monoid (KVLink n r c) where
    mempty = KVLink mempty mempty mempty mempty mempty False
    mappend (KVLink i r c d a v) (KVLink i' r' c' d' a' v') =
        KVLink i'' r'' c'' d'' a'' v''
        where
            i'' = mappend i i'
            r'' = mappend r r'
            c'' = mappend c c'
            d'' = mappend d d'
            a'' = mappend a a'
            v'' = or [v, v']
instance (Ord n, Ord r, Ord c) => Eq (KVLink n r c) where
    a == b = and [i == i', r == r', c == c']
        where
            i  = linkIdentifier a
            i' = linkIdentifier b
            r  = linkRelations a
            r' = linkRelations b
            c  = linkClasses a
            c' = linkClasses b
