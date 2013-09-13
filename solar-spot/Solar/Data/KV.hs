module Solar.Data.KV where

import Data.Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

type KVClass c = c

data KVIdentifier n = KVIdentifier
    { namespace :: !n
    , key       :: !Text
    } deriving (Show)

data KVLink n r c = KVLink
    { linkIdentifier :: !(KVIdentifier n)
    , linkRelations  :: ![r]
    , linkClasses    :: ![KVClass c]
    } deriving (Show)

data KVMeta namespace relations classes = KVMeta
    { identifier   :: !(KVIdentifier namespace)
    -- ^ Identifier for this entity
    , classes      :: ![KVClass classes]
    -- ^ The classifications 
    , relations    :: ![KVLink namespace relations classes]
    , lastModified :: !UTCTime
    , invalid      :: !Bool
    }

instance (Show a, Show b, Show c) => Show (KVMeta a b c) where
    show (KVMeta a b c d e)=
        "KVMeta {identifier = "
        ++ show a
        ++ ", classes = "
        ++ show b
        ++ ", relations = "
        ++ show c
        ++ ", lastModified = "
        ++ t
        ++ ", invalid = "
        ++ show e
        ++ "}"
        where
            t = formatTime defaultTimeLocale "%FT%T" d


data KV namespace relations classes datas cache = KV
    { content       :: !(datas namespace relations classes)
    , meta          :: !(KVMeta namespace relations classes)
    , caches        :: !(Maybe (cache namespace relations classes))
    }
