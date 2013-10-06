{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
module Solar.Data.KV.Link where

import GHC.Generics as G
import Data.Generics as D

import Data.Time.Format()
import Data.Time.Clock (UTCTime(..))

import Solar.Data.KV.Identifier
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
    } deriving (Show, Read, Typeable, Data, G.Generic, Eq, Ord)