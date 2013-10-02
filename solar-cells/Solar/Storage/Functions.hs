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
