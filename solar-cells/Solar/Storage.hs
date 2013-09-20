{-# LANGUAGE Rank2Types #-}
module Solar.Storage where

import           Solar.Data.KV as K
import           Data.Default.Class
import qualified Data.Foldable as F
import qualified Control.Monad as M

data KVStorage r n c d c' = KVStorage
    { getPersistant :: (Monad m) => KVIdentifier n -> m (Maybe (KV r n c d c'))
    , getCached     :: (Monad m) => KVIdentifier n -> m (Maybe (KV r n c d c'))
    , putPersistant :: (Monad m) => KV r n c d c' -> m (KV r n c d c')
    , putCached     :: (Monad m) => KV r n c d c' -> m ()
    , delPersistant :: (Monad m) => KVIdentifier n -> m Bool
    , delCached     :: (Monad m) => KVIdentifier n -> m Bool
    }

instance Default (KVStorage r n c d c') where
    def = KVStorage
        { getPersistant = \_ -> return Nothing
        , getCached = \_ -> return Nothing
        , putPersistant = return
        , putCached = \_ -> return ()
        , delPersistant = \_ -> return False
        , delCached = \_ -> return False
        }

put :: (Monad m) => KVStorage r n c d c' -> KV r n c d c' -> m ()
put store kv = do
    kv' <- putPersistant store kv
    -- ↑ other storages may modify the kv, such as
    -- riak, since it may resolve merge conflicts. 
    putCached store kv'
{-# INLINABLE put #-}

get :: (Monad m) => KVStorage r n c d c' -> KVIdentifier n -> m (Maybe (KV r n c d c'))
get store i = do
    ckv <- getCached store i
    case ckv of
        Just (kv) -> return ckv
        Nothing -> do
            pkv <- getPersistant store i
            F.forM_ pkv (\v -> putCached store v)
            return pkv
{-# INLINABLE get #-}

del ::  (Monad m) => KVStorage r n c d c' -> KVIdentifier n -> m Bool
del store i =
    M.liftM or $ sequence [c, p]
    where
        p = delPersistant store i
        c = delCached store i
{-# INLINABLE del #-}

invalidate :: (Monad m) => KVStorage r n c d c' -> KVIdentifier n -> m ()
invalidate store i =
    get store i >>= F.mapM_ (\v -> put store (K.invalidate v))
{-# INLINABLE invalidate #-}