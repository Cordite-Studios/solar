{-# LANGUAGE Rank2Types #-}
module Solar.Storage
    (
    -- * Types
      Context
    , Storage(..)
    -- * Manipulators
    , get
    , put
    , del
    , Solar.Storage.invalidate
    -- * Helpers
    , noContext
    )
where

import           Solar.Data.KV as K
import           Data.Default.Class
import qualified Data.Foldable as F
import qualified Control.Monad as M
import qualified Data.Typeable as T
import qualified Data.Dynamic  as D
import qualified Data.Map      as Map

-- | Contexts are needed for instances like riak
-- which comes with a vector clock.
-- 
-- It may also be used to store connections or
-- pools.
type Context = Map.Map T.TypeRep D.Dynamic

data Storage r n c d c' = Storage
    { getPersistant :: (Monad m) => Context -> KVIdentifier n -> m (Maybe (KV r n c d c', Context))
    -- ^ Given an identifier, provide a result from the
    -- persistant medium
    , getCached     :: (Monad m) => Context -> KVIdentifier n -> m (Maybe (KV r n c d c', Context))
    -- ^ Given an identifier, lookup in the cache the value
    , putPersistant :: (Monad m) => Context -> KV r n c d c' ->  m (KV r n c d c', Context)
    -- ^ Put this content into the persistant medium
    , putCached     :: (Monad m) => Context -> KV r n c d c' -> m ()
    -- ^ Put this content into the cache medium
    , delPersistant :: (Monad m) => Context -> KVIdentifier n -> m Bool
    -- ^ Permanently remove (not 'invalidate') the entity
    -- from the persistant medium
    , delCached     :: (Monad m) => Context -> KVIdentifier n -> m Bool
    -- ^ Clear this entry from the cache (not 'invalidate')
    }

instance Default (Storage r n c d c') where
    def = Storage
        { getPersistant = \_ _ -> return Nothing
        , getCached = \_ _ -> return Nothing
        , putPersistant = \a b -> return (b, a)
        , putCached = \_ _ -> return ()
        , delPersistant = \_ _ -> return False
        , delCached = \_ _ -> return False
        }

put :: (Monad m) => Context -> Storage r n c d c' -> KV r n c d c' -> m ()
put c' store kv = do
    M.when (Map.size c' == 0) $ fail failMessage
    (kv', c) <- putPersistant store c' kv
    -- ↑ other storages may modify the kv, such as
    -- riak, since it may resolve merge conflicts. 
    putCached store (Map.union c c') kv'
{-# INLINABLE put #-}

get :: (Monad m) => Context -> Storage r n c d c' -> KVIdentifier n -> m (Maybe (KV r n c d c', Context))
get c' store i = do
    M.when (Map.size c' == 0) $ fail failMessage
    ckv <- getCached store c' i
    case ckv of
        Just (kv, _) -> return ckv
        Nothing -> do
            pkv <- getPersistant store c' i
            F.forM_ pkv (\(v, c) -> putCached store (Map.union c c') v)
            return pkv
{-# INLINABLE get #-}

del ::  (Monad m) => Context -> Storage r n c d c' -> KVIdentifier n -> m Bool
del c store i =
    M.liftM or $ sequence [c', p]
    where
        p = delPersistant store c i
        c' = delCached store c i
{-# INLINABLE del #-}

invalidate :: (Monad m) => Context -> Storage r n c d c' -> KVIdentifier n -> m ()
invalidate c' store i =
    get c' store i >>= F.mapM_ (\(v, c) -> put (Map.union c c') store (K.invalidate v))
{-# INLINABLE invalidate #-}

noContext :: Context
noContext = Map.empty
{-# INLINABLE noContext #-}

failMessage = "Empty Context Provided, no possible outcome!"
{-# INLINABLE failMessage #-}
