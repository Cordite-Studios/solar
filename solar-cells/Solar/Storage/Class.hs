{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Has all the type classes which are used in solar cells
-}
module Solar.Storage.Class where

import           Solar.Data.KV as K
import           Solar.Storage.Types

-- | Main typeclass that the Storage system must have implementations for.
--
-- This does not run within another monad.
class (Monad m) => StorageFC n r c d c' m a where
    -- | Uses the implictly available data structure to get the functions
    -- for this type, and execute it against this information to get a
    -- 'KV' if available and correctly deserialized.
    sfcGet  :: ()
            => a -- ^ The current data structure in question
            -> Context -- ^ The context
            -> KVIdentifier n -- ^ The identifier of the 'KV' desired
            -> m (Maybe (KV n r c d c'), Context) 
            -- ^ The monadic action to take wherein the 'KV' might be
            -- found, and the 'Context' state may be modified.

    -- | Uses the implictly available data structure to get the functions
    -- for this type, and execute it against this information to persist
    -- a 'KV'. If it happens to change, such as in the case of conflict
    -- resolution, then here it is given back. 
    sfcPut  :: ()
            => a -- ^ The current data structure in question
            -> Context -- ^ The context
            -> KV n r c d c' -- ^ The 'KV' to store
            -> m (KV n r c d c', Context)
            -- ^ The monadic action to take where the 'KV' is
            -- stored, and if anything happens to the 'KV', it
            -- is also given back. The 'Context' state is also
            -- given back.


    -- | Uses the implictly available data structure to get the functions
    -- for this type, and execute it against this information to delete
    -- an entity from storage. It returns if it has failed or not.
    sfcDel  :: ()
            => a -- ^ The current data structure in question
            -> Context -- ^ The context
            -> TaggedIdentifier n r c (d n r c) (c' n r c)
            -- ^ A fully specified type equivalent with an
            -- identifier.
            -> m (Bool, Context)
            -- ^ The monadic action to take where the 'KV' by
            -- the identifier is deleted, and a success flag
            -- is returned, along with the 'Context' if it has
            -- changed.