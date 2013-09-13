{-# LANGUAGE DeriveDataTypeable #-}
module Solar.Data.KV
    ( KVClass
    , KVTime(..)
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

type KVClass c = c
type KVTime    = UTCTime 

data KVIdentifier n = KVIdentifier
    { namespace :: !n
    , key       :: !Text
    } deriving (Show, Read)

data KVLink n r c = KVLink
    { linkIdentifier :: !(KVIdentifier n)
    , linkRelations  :: ![r]
    , linkClasses    :: ![KVClass c]
    } deriving (Show, Read)

data KVMeta namespace relations classes = KVMeta
    { identifier   :: !(KVIdentifier namespace)
    -- ^ Identifier for this entity
    , classes      :: ![KVClass classes]
    -- ^ The classifications 
    , relations    :: ![KVLink namespace relations classes]
    , lastModified :: !KVTime
    , invalid      :: !Bool
    } deriving (Show, Read)

data KV namespace relations classes datas cache = KV
    { content       :: !(datas namespace relations classes)
    , meta          :: !(KVMeta namespace relations classes)
    , caches        :: !(Maybe (cache namespace relations classes))
    } deriving (Show, Read)

data KVNoCache n r c = KVNoCache deriving (Show, Read, Typeable, Eq, Ord)

kvNoCache :: (Maybe (KVNoCache n r c))
kvNoCache = Nothing