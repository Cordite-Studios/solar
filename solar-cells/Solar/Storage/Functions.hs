{-# LANGUAGE FlexibleContexts #-}
{- |
Provides the primary functons for the storage system
-}
module Solar.Storage.Functions
where

import           Solar.Data.KV as K
import           Solar.Storage.Types
import           Solar.Storage.Class
import           Control.Monad.Trans.RWS as R
import           Control.Monad.Trans.Class (lift)
import           Data.Monoid(Monoid(..))


-- | Retrieves the 'KV' within the 'RWST' environment
getSF   :: (StorageFC n r c d c' m a, Monad m, Monoid b)
        => KVIdentifier n -- ^ The identifier of the 'KV' desired
        -> RWST a b Context m (Maybe (KV n r c d c'))
        -- ^ Results in the action that might have a 'KV' value by
        -- that identifier.
getSF i = do
    c <- get
    s <- ask
    (v, c') <- lift $ sfcGet s c i
    put c'
    return v

-- | Persists the 'KV' within the 'RWST' environment
putSF   :: (StorageFC n r c d c' m a, Monad m, Monoid b)
        => KV n r c d c' -- ^ The 'KV' that you want to put up
        -> RWST a b Context m (KV n r c d c')
        -- ^ The resulting action. It may be important
        -- to have a 'Context' that was set by a previous
        -- 'getSF' call.
putSF kv = do
    c <- get
    s <- ask
    (v, c') <- lift $ sfcPut s c kv
    put c'
    return v

-- | Deletes the 'KV' within the 'RWST' environment
delSF    :: (StorageFC n r c d c' m a, Monad m, Monoid b)
        => TaggedIdentifier n r c (d n r c) (c' n r c)
        -- ^ A type-tagged identifier for this entity.
        -- The types may be significant.
        -> RWST a b Context m (Bool)
        -- ^ The resulting action with a success flag.
delSF i = do
    c <- get
    s <- ask
    (v, c') <- lift $ sfcDel s c i
    put c'
    return v

