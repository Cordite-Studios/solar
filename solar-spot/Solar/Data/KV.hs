{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solar.Data.KV
    ( KVTime(..)
    , KVLink(..)
    , KVMeta(..)
    , KV(..)
    , KVIdentifier(..)
    , KVNoCache(..)
    , kvNoCache
    , KVDirection(..)
    )
    where

import Data.Text
import Data.Time.Clock (UTCTime(..))
import Data.Typeable
import Data.Time.Format (formatTime, readsTime, ParseTime(..))
import System.Locale (defaultTimeLocale)
import Solar.Data.Graph.Direction

type KVTime    = UTCTime 

data KVIdentifier n = KVIdentifier
    { namespace :: !n
    -- ^ Namespace enumeration for where this belongs
    , key       :: !Text
    -- ^ Textual name of the key that can be looked up
    } deriving (Show, Read, Typeable)

data KVLink n r c = KVLink
    { linkIdentifier :: !(KVIdentifier n)
    -- ^ Identity of the remote entity
    , linkRelations  :: ![r]
    -- ^ Relation Enums that describe what it means
    , linkClasses    :: ![c]
    -- ^ Class Enums that describe what the entity is
    , linkDirection  :: !KVDirection
    -- ^ Stores the direction of this entity to the remote entity.
    , linkAdded      :: !KVTime
    -- ^ When this link was added
    , linkInvalid    :: !Bool
    -- ^ Invalidation flag
    } deriving (Show, Read, Typeable)

data KVMeta namespace relations classes = KVMeta
    { identifier            :: !(KVIdentifier namespace)
    -- ^ Identifier for this entity
    , classes               :: ![classes]
    -- ^ The classifications for this entity
    , relations             :: ![KVLink namespace relations classes]
    -- ^ Links that this entity has with others
    , lastModified          :: !KVTime
    -- ^ Modification date for the entire data element
    , lastModifiedContent   :: !KVTime
    -- ^ Content Modification Date
    , createdAt             :: !KVTime
    -- ^ When this entity was created
    , invalid               :: !Bool
    -- ^ Invalidation flag
    } deriving (Show, Read, Typeable)

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

-- Time for class implementations

instance (Typeable a, Typeable b, Typeable c, Typeable3 d, Typeable3 e) =>
    Typeable (KV a b c d e) where
    typeOf _ =
        mkTyConApp (mkTyCon3 "solar-spot" "Solar.Data.KV" "KV") []
        `mkAppTy`
        typeOf namespace 
        `mkAppTy`
        typeOf relations
        `mkAppTy`
        typeOf classes
        `mkAppTy`
        typeOf datas
        `mkAppTy`
        typeOf cache
        where   
            namespace  = undefined :: a
            relations  = undefined :: b
            classes    = undefined :: c
            datas      = undefined :: d a b c
            cache      = undefined :: e a b c      
