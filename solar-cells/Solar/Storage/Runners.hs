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
        => a -- ^ The storage functions interface
        -> Context -- ^ The initial context
        -> RWST a b Context m z
        -- ^ The monad of actions to perform
        -> m (z, Context, b)
        -- ^ The result includes the monadic result as
        -- as any output from the writer. The state is
        -- also provided as the 'Context' which
        -- likely will change.
runS s c r = runRWST r s c

-- | Helful for operations that make changes, but
-- do not result in any value. Deletes are fine here
-- though the success would have to be accounted for
-- manually within the monad.
runS'   :: (StorageFC n r c d c' m a, Monad m)
        => a -- ^ The storage functions interface
        -> Context -- ^ The initial context
        -> RWST a () Context m ()
        -- ^ The monad of actions to perform
        -> m (Context)
        -- ^ The result of the monadic expression,
        -- only giving back the 'Context' state.
runS' s c r = do
    (_, c', _) <- runRWST r s c
    return (c')

-- | Assumes that the Writer portion is '()' and
-- thus has no reason to come back out.
-- The resulting value is what is returned with
-- the 'Context'
runU    :: (StorageFC n r c d c' m a, Monad m)
        => a -- ^ The storage functions interface
        -> Context -- ^ The initial context
        -> RWST a () Context m z
        -- ^ The monad of actions to perform that may
        -- not utilize the Writer monad portion of
        -- the 'RWST' monad transformer
        -> m (z, Context)
        -- ^ The result of the monadic expression
        -- with the 'Context' state.
runU s c r = do
    (v, c', _) <- runRWST r s c
    return (v, c')

-- | Run the request and return the value according to the
-- Writer monad with 'tell'. However, the 'Context'
-- is still available for later reads.
runW  :: (StorageFC n r c d c' m a, Monad m, Monoid b)
        => a -- ^ The storage functions interface
        -> Context -- ^ The initial context
        -> RWST a b Context m ()
        -- ^ The monad of actions to perform. No
        -- monadic result may be given directly,
        -- however the writer monad portion is
        -- available to be used.
        -> m (b, Context)
        -- ^ The result of the monadic expression
        -- which gives out the completed 'Monoid'
        -- value from the writer monad.
runW s c r = do
    (_, c', v) <- runRWST r s c
    return (v, c')

-- | Uses the writer monad part using 'tell', where
-- the context doesn't matter. This can be useful for
-- doing a one time read for the entire request.
runW'  :: (StorageFC n r c d c' m a, Monad m, Monoid b)
        => a -- ^ The storage functions interface
        -> Context -- ^ The initial context
        -> RWST a b Context m ()
        -- ^ The monad of actions to perform
        -- with the writer portion used.
        -> m (b)
        -- ^ The result of the monadic expression, 
        -- discarding the 'Context' state! Do not
        -- use this if you plan perform any action
        -- latter in the request!
runW' s c r = do
    (_, _, v) <- runRWST r s c
    return (v)