{- |
Gives utility running functions for managing how provided monads are ran.
-}
module Solar.Storage.Runners where

import           Solar.Storage.Types
import           Solar.Storage.Class
import           Control.Monad.Trans.RWS as R
import           Data.Monoid(Monoid(..))

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