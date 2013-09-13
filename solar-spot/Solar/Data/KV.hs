{-# LANGUAGE DeriveDataTypeable #-}
module Solar.Data.KV
    ( KVTime(..)
    , KVLink(..)
    , KVMeta(..)
    , KV(..)
    , KVIdentifier(..)
    , KVNoCache(..)
    , kvNoCache
    )
    where

import Data.Text
import Data.Time.Clock (UTCTime(..))
import Data.Typeable
import Data.Time.Format (formatTime, readsTime, ParseTime(..))
import System.Locale (defaultTimeLocale)

type KVTime    = UTCTime 

data KVIdentifier n = KVIdentifier
    { namespace :: !n
    -- ^ Namespace enumeration for where this belongs
    , key       :: !Text
    -- ^ Textual name of the key that can be looked up
    } deriving (Show, Read)

data KVLink n r c = KVLink
    { linkIdentifier :: !(KVIdentifier n)
    -- ^ Identity of the remote entity
    , linkRelations  :: ![r]
    -- ^ Relation Enums that describe what it means
    , linkClasses    :: ![c]
    -- ^ Class Enums that describe what the entity is
    } deriving (Show, Read)

data KVMeta namespace relations classes = KVMeta
    { identifier   :: !(KVIdentifier namespace)
    -- ^ Identifier for this entity
    , classes      :: ![classes]
    -- ^ The classifications for this entity
    , relations    :: ![KVLink namespace relations classes]
    -- ^ Links that this entity has with others
    , lastModified :: !KVTime
    -- ^ Modification date for the entire data element
    , invalid      :: !Bool
    -- ^ Invalidation flag
    } deriving (Show, Read)

data KV namespace relations classes datas cache = KV
    { meta          :: !(KVMeta namespace relations classes)
    -- ^ Meta data for this entity
    , content       :: !(datas namespace relations classes)
    -- ^ Content for this entity
    , caches        :: !(Maybe (cache namespace relations classes))
    -- ^ Caches, optional, use 'kvNoCache' function or 'KVNoCache'
    -- data type if you don't plan for this entity to ever have
    -- caches.
    } deriving (Show, Read)

-- | The "No Cache" data type, use 'kvNoCache' to provide a typed 'Nothing'
data KVNoCache n r c = KVNoCache
    deriving (Show, Read, Typeable, Eq, Ord)

-- | Gives a typed 'Nothing' of 'KVNoCache'
kvNoCache :: (Maybe (KVNoCache n r c))
kvNoCache = Nothing