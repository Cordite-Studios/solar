{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solar.Data.KV.Utilities where

import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar
import Data.Typeable
import Data.Generics as D
import GHC.Generics as G
import Data.Time.Format()
import Data.Monoid

-- | The "No Cache" data type, use 'kvNoCache' to provide a typed 'Nothing'
data KVNoCache n r c = KVNoCache
    deriving (Show, Read, Typeable, Eq, Ord, Data, G.Generic)

-- | Gives a typed 'Nothing' of 'KVNoCache'
kvNoCache :: (Maybe (KVNoCache n r c))
kvNoCache = Nothing

instance Monoid UTCTime where
	mempty = UTCTime (fromGregorian 0 0 0) 0
	mappend u u'
		| u > u' = u
		| u < u' = u'
		| otherwise = u'

instance Monoid (KVNoCache n r c) where
	mempty = KVNoCache
	mappend a _ = a
