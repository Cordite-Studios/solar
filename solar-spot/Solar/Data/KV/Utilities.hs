{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
module Solar.Data.KV.Utilities where

import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar
import Data.Typeable
import Data.Generics as D
import GHC.Generics as G
import Data.Time.Format()

-- | The "No Cache" data type, use 'kvNoCache' to provide a typed 'Nothing'
data KVNoCache n r c = KVNoCache
    deriving (Show, Read, Typeable, Eq, Ord, Data, G.Generic)

-- | Gives a typed 'Nothing' of 'KVNoCache'
kvNoCache :: (Maybe (KVNoCache n r c))
kvNoCache = Nothing

emptyTime :: UTCTime
emptyTime = UTCTime (fromGregorian 0 0 0) 0