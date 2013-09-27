{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Typeclass helpers for interacting with stored data.

These should be per-storage solution, it may be
a pipe of different solutions.
-}
module Solar.Storage.Class
    ( -- * Main classes
      StorageC(..)
    , StorageCS(..)
    , getKV
    , putKV
    , delKV
    , delKVT
    )
where


import           Solar.Data.KV as K
import           Solar.Storage.Types
import           Control.Monad.Trans.State as ST
import           Control.Monad.Trans.Reader as RT
import           Control.Monad.Trans.Class (lift)


-- | Storage type class for how to operate given a context for a
-- a specific storage type.
--
-- You may implement 'StorageCS' and still use this interface.
class (Monad m) => StorageC n r c d c' m where
    -- | Puts the 'KV' into the storage, may merge
    -- and result in a different 'KV'.
    putC :: Context
         -> KV n r c d c' -- ^ Key-Value to store
         -> m (KV n r c d c', Context)
         -- ^ In case the 'KV' changed (like merging)
         -- The 'Context' is also given back since it may
         -- change as well.

    -- | Gets the 'KV' according to the identifier given
    getC :: Context 
         -> KVIdentifier n -- ^ Identity of the 'KV'
         -> m (Maybe (KV n r c d c'), Context)
         -- ^ Returns a 'KV' if found (and parsed correctly)
         -- and the context, since it may change.

    -- | Depends on 'delCT' if not implemented
    delC :: Context
         -> KV n r c d c' -- ^ 'KV' to delete
         -> m (Bool, Context)
         -- ^ True on successful deletion.
         -- Context may change, so it is given back.

    -- | Depends on 'delC' if not implemented.
    -- It is best to implement this one for efficiency reasons!
    delCT :: Context
          -> TaggedIdentifier n r c (d n r c) (c' n r c)
          -- ^ Tagged identity to witness the types
          -> m (Bool, Context)
          -- ^ True on successful deletion.
          -- Context may change, so it is given back.

    -- Default implementations
    delC c kv = delCT c i
        where
            i :: TaggedIdentifier n r c (d n r c) (c' n r c)
            i = TaggedIdentifier $ identifier.meta $ kv
    
    delCT c i = do
        (kv', c') <- getC c $ untagIdentifier i
        case kv' of
            Just (kv :: KV n r c d c') -> delC c' kv
            Nothing -> return (False, c)

-- | Storage type class for how to operate given a context for a
-- a specific storage type. Operates in the 'StateT' monad.
--
-- You may implement 'StorageC' and still use this interface.
class (Monad m) => StorageCS n r c d c' m where
    -- | Puts the entity into the storage, may
    -- result in a different 'KV', so it is given back
    -- too.
    putCS :: KV n r c d c' -- ^ Key-Value to store.
          -> StateT Context m (KV n r c d c')
          -- ^ Returns a 'KV', since it might change,
          -- such as in the case of a merge.
          -- The state may change.

    -- | Gets the identity according to the identifier given.
    getCS :: KVIdentifier n -- ^ Key-Value Identifier to lookup with
          -> StateT Context m (Maybe (KV n r c d c'))
          -- ^ Possibly returns a 'KV' if it was found and
          -- could be correctly parsed according to the
          -- type
          -- The state may change.

    -- | Depends on 'delCST' if not implemented.
    delCS :: KV n r c d c' -- ^ Key-Value to remove
          -> StateT Context m (Bool)
          -- ^ Returns True on successful deletion.
          -- The state may change.

    -- | Depends on 'delCS' if not implemented.
    -- It is best to implement this one for efficiency reasons!
    delCST :: TaggedIdentifier n r c (d n r c) (c' n r c)
           -- ^ Witnesses the types and contains a 'KVIdentifier'
           -> StateT Context m (Bool)
           -- ^ Returns True on successful deletion.
           -- The state may change.


    -- Default implementations
    
    delCS kv = delCST i
        where
            i :: TaggedIdentifier n r c (d n r c) (c' n r c)
            i = TaggedIdentifier $ identifier.meta $ kv
    delCST i = do
        kv' <- getCS $ untagIdentifier i
        case kv' of
            Just (kv :: KV n r c d c') -> delCS kv
            Nothing                    -> return False


wrapKV v f = do
    (a, c) <- get
    let st = f v
        st' = runStateT st c
    (r, c') <- lift $ st'
    put (a, c')
    return r

getKV   :: (Monad m)
        => KVIdentifier n
        -> StateT (a, Context) m (Maybe (KV n r c d c'))
getKV i = wrapKV i getCS

putKV   :: (Monad m)
        => KV n r c d c'
        -> StateT (a, Context) m (KV n r c d c')
putKV kv = wrapKV kv putCS

delKV   :: (Monad m)
        => KV n r c d c'
        -> StateT (a, Context) m (Bool)
delKV kv = wrapKV kv delCS

delKVT  :: (Monad m)
        => TaggedIdentifier n r c (d n r c) (c' n r c)
        -> StateT (a, Context) m (Bool)
delKVT i = wrapKV i delCST

-- Instances 

instance (StorageCS n r c d c' m) => StorageC n r c d c' m where
    putC c kv = runStateT (putCS kv) c
    getC c i  = runStateT (getCS i) c
    delC c kv = runStateT (delCS kv) c
    delCT c i = runStateT (delCST i) c

wrapS v f = do
    c <- get
    (kv', c') <- f c v
    put c'
    return kv'

instance (StorageC n r c d c' m) => StorageCS n r c d c' m where
    putCS kv = wrapS kv putC
    getCS i = wrapS i getC
    delCS kv = wrapS kv delC
    delCST i = wrapS i delCT

