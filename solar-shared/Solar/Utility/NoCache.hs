{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Solar.Utility.NoCache where

import Data.Typeable
import Data.Generics as D
import GHC.Generics as G
import Data.Monoid

-- | The "No Cache" data type, use 'kvNoCache' to provide a typed 'Nothing'
data KVNoCache n r c = KVNoCache
    deriving (Show, Read, Typeable, Eq, Ord, Data, G.Generic)

-- | Gives a typed 'Nothing' of 'KVNoCache'
kvNoCache :: (Maybe (KVNoCache n r c))
kvNoCache = Nothing

instance Monoid (KVNoCache n r c) where
    mempty = KVNoCache
    mappend a _ = a