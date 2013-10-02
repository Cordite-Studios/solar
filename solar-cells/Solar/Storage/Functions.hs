{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Provides the primary functons for the storage system
-}
module Solar.Storage.Functions
    (
    -- * Data Structure
      StorageFC(..)
    -- * Stateful Manipulators
    , getSF
    , putSF
    , delSF
    -- * Runners
    , runS
    , runS'
    , runW
    , runW'
    , runU
    -- * Re-exports
    , R.tell
    , lift
    )
where

import           Solar.Data.KV as K
import           Solar.Storage.Types
import           Control.Monad.Trans.RWS as R
import           Control.Monad.Trans.Class (lift)
import           Data.Monoid(Monoid(..))

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


-- | Retrieves the 'KV' within the 'RWST' environment
getSF   :: (StorageFC n r c d c' m a, Monad m, Monoid b)
        => KVIdentifier n
        -> RWST a b Context m (Maybe (KV n r c d c'))
getSF i = do
    c <- get
    s <- ask
    (v, c') <- lift $ sfcGet s c i
    put c'
    return v

-- | Persists the 'KV' within the 'RWST' environment
putSF   :: (StorageFC n r c d c' m a, Monad m, Monoid b)
        => KV n r c d c'
        -> RWST a b Context m (KV n r c d c')
putSF kv = do
    c <- get
    s <- ask
    (v, c') <- lift $ sfcPut s c kv
    put c'
    return v

-- | Deletes the 'KV' within the 'RWST' environment
delSF    :: (StorageFC n r c d c' m a, Monad m, Monoid b)
        => TaggedIdentifier n r c (d n r c) (c' n r c)
        -> RWST a b Context m (Bool)
delSF i = do
    c <- get
    s <- ask
    (v, c') <- lift $ sfcDel s c i
    put c'
    return v

-- | Allows for easier use of the stateful functions,
-- gives back all the kinds of results from the outer
-- monad and the writer monad.
-- The 'Context' is also given back.
runS    :: (StorageFC n r c d c' m a, Monad m, Monoid b)
        => a
        -> Context
        -> RWST a b Context m z
        -> m (z, Context, b)
runS s c r = runRWST r s c

-- | Helful for operations that make changes, but
-- do not result in any value. Deletes are fine here
-- though the success would have to be accounted for
-- manually within the monad.
runS'   :: (StorageFC n r c d c' m a, Monad m)
        => a
        -> Context
        -> RWST a () Context m ()
        -> m (Context)
runS' s c r = do
    (_, c', _) <- runRWST r s c
    return (c')

-- | Assumes that the Writer portion is '()' and
-- thus has no reason to come back out.
-- The resulting value is what is returned with
-- the 'Context'
runU    :: (StorageFC n r c d c' m a, Monad m)
        => a
        -> Context
        -> RWST a () Context m z
        -> m (z, Context)
runU s c r = do
    (v, c', _) <- runRWST r s c
    return (v, c')

-- | Run the request and return the value according to the
-- Writer monad with 'tell'. However, the 'Context'
-- is still available for later reads.
runW  :: (StorageFC n r c d c' m a, Monad m, Monoid b)
        => a
        -> Context
        -> RWST a b Context m ()
        -> m (b, Context)
runW s c r = do
    (_, c', v) <- runRWST r s c
    return (v, c')

-- | Uses the writer monad part using 'tell', where
-- the context doesn't matter. This can be useful for
-- doing a one time read for the entire request.
runW'  :: (StorageFC n r c d c' m a, Monad m, Monoid b)
        => a
        -> Context
        -> RWST a b Context m ()
        -> m (b)
runW' s c r = do
    (_, _, v) <- runRWST r s c
    return (v)