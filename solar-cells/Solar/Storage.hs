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
    }

instance Default (KVStorage r n c d c') where
    def = KVStorage
        { getPersistant = \_ -> return Nothing
        , getCached = \_ -> return Nothing
        , putPersistant = return
        , putCached = \_ -> return ()
        }

put :: (Monad m) => KVStorage r n c d c' -> KV r n c d c' -> m ()
put store kv = do
    kv' <- putPersistant store kv
    -- ^ other storages may modify the kv, such as
    -- riak, since it may resolve merge conflicts. 
    putCached store kv'

get :: (Monad m) => KVStorage r n c d c' -> KVIdentifier n -> m (Maybe (KV r n c d c'))
get store i = do
    ckv <- getCached store i
    case ckv of
        Just (kv) -> return ckv
        Nothing -> do
            pkv <- getPersistant store i
            F.forM_ pkv (\v -> putCached store v)
            return pkv

invalidate :: (Monad m) => KVStorage r n c d c' -> KVIdentifier n -> m ()
invalidate store i =
    get store i >>= F.mapM_ (\v -> put store (K.invalidate v))