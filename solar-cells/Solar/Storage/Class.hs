{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Has all the type classes which are used in solar cells
-}
module Solar.Storage.Class where

import           Solar.Data.KV as K
import           Solar.Storage.Types

-- | Main typeclass that the Storage system must have implementations for.
class (Monad m) => StorageFC n r c d c' m a where
    -- | Uses the implictly available data structure to get the functions
    -- for this type, and execute it against this information to get a
    -- 'KV' if available and correctly deserialized.
    sfcGet :: a -> Context -> KVIdentifier n -> m (Maybe (KV n r c d c'), Context) 
    -- | Uses the implictly available data structure to get the functions
    -- for this type, and execute it against this information to persist
    -- a 'KV'. If it happens to change, such as in the case of conflict
    -- resolution, then here it is given back. 
    sfcPut :: a -> Context -> KV n r c d c' -> m (KV n r c d c', Context)
    -- | Uses the implictly available data structure to get the functions
    -- for this type, and execute it against this information to delete
    -- an entity from storage. It returns if it has failed or not.
    sfcDel :: a -> Context -> TaggedIdentifier n r c (d n r c) (c' n r c) -> m (Bool, Context)