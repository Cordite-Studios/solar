{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
module Solar.Storage.Functions
    (
    -- * Data Structure
      StorageFn(..)
    , StorageF(..)
    -- * Stateful Manipulators
    , runSF
    , getSF
    , putSF
    , delSF
    -- * Explicit State Manipulators
    , getF
    , putF
    , delF
    )
where

import           Solar.Data.KV as K
import           Solar.Storage.Types
import           Control.Monad.Trans.State as ST
import           Control.Monad.Trans.Class (lift)


-- | A function with resolved functions for this
-- specific type of 'KV'
data StorageFn n r c d c' m = StorageFn
    { -- | The getter for this specific 'KV' type
      sfGet :: ()
            => Context 
            -> KVIdentifier n
            -> m (Maybe (KV n r c d c'), Context) 
    , -- | Persist this specific 'KV' type
      sfPut :: ()
            => Context
            -> KV n r c d c'
            -> m (KV n r c d c', Context)
    , -- | Delete this specific 'KV' type
      sfDel :: ()
            => Context
            -> TaggedIdentifier n r c (d n r c) (c' n r c)
            -> m (Bool, Context)
    }
-- | Allows an arbitrary structure to have the functions for this
-- kind of 'KV' be explicit
class (Monad m) => StorageF n r c d c' m a where
    convertSF   :: ()
                => a -- ^ The host storage structure for these functions
                -> StorageFn n r c d c' m
                -- ^ The resulting structure with explicitly known functions

-- | Uses the implictly available data structure to get the functions
-- for this type, and execute it against this information to get a
-- 'KV' if available and correctly deserialized.
getF    :: (StorageF n r c d c' m a, ?s :: a, Monad m)
        => Context
        -> KVIdentifier n
        -> m (Maybe (KV n r c d c'), Context)
getF c i = sfGet s c i
    where s = convertSF ?s

-- | Uses the implictly available data structure to get the functions
-- for this type, and execute it against this information to persist
-- a 'KV'. If it happens to change, such as in the case of conflict
-- resolution, then here it is given back. 
putF    :: (StorageF n r c d c' m a, ?s :: a, Monad m)
        => Context
        -> KV n r c d c'
        -> m (KV n r c d c', Context)
putF c kv = sfPut s c kv
    where s = convertSF ?s

-- | Uses the implictly available data structure to get the functions
-- for this type, and execute it against this information to delete
-- an entity from storage. It returns if it has failed or not.
delF    :: (StorageF n r c d c' m a, ?s :: a, Monad m)
        => Context
        -> TaggedIdentifier n r c (d n r c) (c' n r c)
        -> m (Bool, Context)
delF c i' = sfDel s c i'
    where s = convertSF ?s

wrapSF :: (Monad m) => v -> (Context -> v -> m (b, Context)) -> StateT Context m b
wrapSF i f = do
    c <- get
    (v, c') <- lift $ f c i
    put c'
    return v

-- | Retrieves the 'KV' within the 'StateT' environment
getSF   :: (StorageF n r c d c' m a, ?s :: a, Monad m)
        => KVIdentifier n
        -> StateT Context m (Maybe (KV n r c d c'))
getSF i = wrapSF i getF

-- | Persists the 'KV' within the 'StateT' environment
putSF   :: (StorageF n r c d c' m a, ?s :: a, Monad m)
        => KV n r c d c'
        -> StateT Context m (KV n r c d c')
putSF kv = wrapSF kv putF

-- | Deletes the 'KV' within the 'StateT' environment
delSF    :: (StorageF n r c d c' m a, ?s :: a, Monad m)
        => TaggedIdentifier n r c (d n r c) (c' n r c)
        -> StateT Context m (Bool)
delSF i = wrapSF i delF

-- | Allows for easier use of the stateful functions
runSF   :: (StorageF n r c d c' m a, ?s :: a, Monad m)
        => Context
        -> StateT Context m z
        -> m (z, Context)
runSF c s = runStateT s c