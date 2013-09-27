{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Solar.Storage.State
    (
    -- * Types
      Context
    , Storage(..)
    -- * Manipulators
    , getS
    , getS'
    , putS
    , putS'
    , delS
    , delS'
    , invalidateS
    , invalidateS'
    -- * Helpers
    , noContext
    , contextWrap
    , addToContext
    , (~+=:)
    )
where

import           Solar.Storage.Types
import           Solar.Storage.Context
import           Solar.Data.KV as K
import           Data.Default.Class
import qualified Data.Foldable as F
import qualified Control.Monad as M
import qualified Data.Typeable as T
import Control.Monad.Trans.State as ST



data Storage n r c d c' = Storage
    { getPersistant :: KVIdentifier n -> StateT Context IO (Maybe (KV n r c d c'))
    -- ^ Given an identifier, provide a result from the
    -- persistant medium
    , getCached     :: KVIdentifier n -> StateT Context IO (Maybe (KV n r c d c'))
    -- ^ Given an identifier, lookup in the cache the value
    , putPersistant :: KV n r c d c' -> StateT Context IO (KV n r c d c')
    -- ^ Put this content into the persistant medium
    , putCached     :: KV n r c d c' -> StateT Context IO ()
    -- ^ Put this content into the cache medium
    , delPersistant :: KVIdentifier n -> StateT Context IO Bool
    -- ^ Permanently remove (not 'invalidate') the entity
    -- from the persistant medium
    , delCached     :: KVIdentifier n -> StateT Context IO Bool
    -- ^ Clear this entry from the cache (not 'invalidate')
    }

instance Default (Storage n r c d c') where
    def = Storage
        { getPersistant = \_ -> return Nothing
        , getCached = \_ -> return Nothing
        , putPersistant = \a -> return a
        , putCached = \_ -> return ()
        , delPersistant = \_ -> return False
        , delCached = \_ -> return False
        }

putS' :: Storage n r c d c' -> KV n r c d c' -> StateT Context IO ()
putS' store kv = do
    kv' <- putPersistant store kv
    -- ↑ other storages may modify the kv, such as
    -- riak, since it may resolve merge conflicts. 
    putCached store kv'
{-# INLINABLE putS' #-}

putS :: (T.Typeable n, T.Typeable r, T.Typeable c, T.Typeable3 d, T.Typeable3 c')
     => KV n r c d c' -- ^ Key value to store
     -> StateT Context IO () -- ^ Resulting action
putS kv = do
    contextWrapS (\s -> putS' s kv) (return ())
{-# INLINABLE putS #-}


getS' :: Storage n r c d c'
      -> KVIdentifier n
      -> StateT Context IO (Maybe (KV n r c d c'))
getS' store i = do
    ckv <- getCached store i
    case ckv of
        Just kv -> return ckv
        Nothing -> do
            pkv <- getPersistant store i
            F.forM_ pkv (\v -> putCached store v)
            return pkv
{-# INLINABLE getS' #-}


-- | Gets an entity from the storage.
-- may add to the context, such as in the
-- anticipated case of riak, context
-- is needed for later when using 'putS'
getS :: (T.Typeable n, T.Typeable r, T.Typeable c, T.Typeable3 d, T.Typeable3 c')
     => KVIdentifier n -- ^ Identifier of the entity
     -> StateT Context IO (Maybe (KV n r c d c'))
     -- ^ Resulting action potentially with an entity
getS i = do
    contextWrapS (\s -> getS' s i) (return Nothing)
{-# INLINABLE getS #-}

-- | Removes a key-value, by identifier given the storage (type is important)
delS' :: Storage n r c d c' -- ^ Storage collection
      -> KVIdentifier n -- ^ Identifier of the item
      -> StateT Context IO Bool -- ^ Resulting action
delS' store i =
    M.liftM or $ sequence [c, p]
    where
        p = delPersistant store i
        c = delCached store i
{-# INLINABLE delS' #-}
delS'' :: Storage n r c d c'
       -> KV n r c d c'
       -> StateT Context IO Bool
delS'' store kv = delS' store $ identifier.meta $ kv
    
{-# INLINABLE delS'' #-}

-- | Removes a key-value from the storage
delS :: (T.Typeable n, T.Typeable r, T.Typeable c, T.Typeable3 d, T.Typeable3 c')
     => KV n r c d c' -- ^ Key value to remove
     -> StateT Context IO Bool -- ^ Resulting action
delS kv = do
    contextWrapS (\s -> delS'' s kv) (return False)
{-# INLINABLE delS #-}

-- | Updates the key-value in the storage to be invalid
-- Must be of the right type to deserialize and reserialize properly
-- 
-- This WILL fetch the 'KV' before updating it. If you already
-- have the value, use 'invalidateS'
invalidateS' :: Storage n r c d c' -> KVIdentifier n -> StateT Context IO ()
invalidateS' store i =
    getS' store i >>= F.mapM_ (\v -> putS' store (K.invalidate v))
{-# INLINABLE invalidateS' #-}

-- | Updates the key-value entity in the storage to be invalid
-- in the meta information
invalidateS :: (T.Typeable n, T.Typeable r, T.Typeable c, T.Typeable3 d, T.Typeable3 c')
            => KV n r c d c' -- ^ Key value to invalidate
            -> StateT Context IO () -- Resulting action to take
invalidateS kv = putS (K.invalidate kv)


-- Class implementations

-- | Does not currently include the type indication of 'm'
-- This may interfear if you include both STM and IO...
storageTypeRepCon = T.mkTyConApp (T.mkTyCon3 "solar-cells" "Solar.Storage" "Storage") []

instance (T.Typeable a, T.Typeable b, T.Typeable c, T.Typeable3 d, T.Typeable3 e) =>
    T.Typeable (Storage a b c d e) where
    typeOf _ =
        storageTypeRepCon
        `T.mkAppTy`
        T.typeOf namespace 
        `T.mkAppTy`
        T.typeOf relations
        `T.mkAppTy`
        T.typeOf classes
        `T.mkAppTy`
        T.typeOf datas
        `T.mkAppTy`
        T.typeOf cache
        where   
            namespace  = undefined :: a
            relations  = undefined :: b
            classes    = undefined :: c
            datas      = undefined :: d a b c
            cache      = undefined :: e a b c
