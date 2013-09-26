{-# LANGUAGE Rank2Types #-}
module Solar.Storage
    (
    -- * Types
      Context
    , Storage(..)
    -- * Manipulators
    , getS
    , putS
    , delS
    , Solar.Storage.invalidate
    -- * Helpers
    , noContext
    , contextWrap
    , addToContext
    , (~+=:)
    )
where

import           Solar.Data.KV as K
import           Data.Default.Class
import qualified Data.Foldable as F
import qualified Control.Monad as M
import qualified Data.Typeable as T
import qualified Data.Dynamic  as D
import qualified Data.Map      as Map
import Control.Monad.Trans.State as ST

-- | Contexts are needed for instances like riak
-- which comes with a vector clock.
-- 
-- It may also be used to store connections or
-- pools.
type Context = Map.Map T.TypeRep D.Dynamic

data Storage n r c d c' = Storage
    { getPersistant :: (Monad m) => KVIdentifier n -> StateT Context m (Maybe (KV n r c d c'))
    -- ^ Given an identifier, provide a result from the
    -- persistant medium
    , getCached     :: (Monad m) => KVIdentifier n -> StateT Context m (Maybe (KV n r c d c'))
    -- ^ Given an identifier, lookup in the cache the value
    , putPersistant :: (Monad m) => KV n r c d c' -> StateT Context m (KV n r c d c')
    -- ^ Put this content into the persistant medium
    , putCached     :: (Monad m) => KV n r c d c' -> StateT Context m ()
    -- ^ Put this content into the cache medium
    , delPersistant :: (Monad m) => KVIdentifier n -> StateT Context m Bool
    -- ^ Permanently remove (not 'invalidate') the entity
    -- from the persistant medium
    , delCached     :: (Monad m) => KVIdentifier n -> StateT Context m Bool
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

putS :: (Monad m) => Storage n r c d c' -> KV n r c d c' -> StateT Context m ()
putS store kv = do
    kv' <- putPersistant store kv
    -- ↑ other storages may modify the kv, such as
    -- riak, since it may resolve merge conflicts. 
    putCached store kv'
{-# INLINABLE putS #-}

getS :: (Monad m) => Storage n r c d c' -> KVIdentifier n -> StateT Context m (Maybe (KV n r c d c'))
getS store i = do
    ckv <- getCached store i
    case ckv of
        Just kv -> return ckv
        Nothing -> do
            pkv <- getPersistant store i
            F.forM_ pkv (\v -> putCached store v)
            return pkv
{-# INLINABLE getS #-}

delS ::  (Monad m) => Storage n r c d c' -> KVIdentifier n -> StateT Context m Bool
delS store i =
    M.liftM or $ sequence [c, p]
    where
        p = delPersistant store i
        c = delCached store i
{-# INLINABLE delS #-}

-- | Updates the record in the database to be invalid
-- Must be of the right type to deserialize and reserialize properly
invalidate :: (Monad m) => Storage n r c d c' -> KVIdentifier n -> StateT Context m ()
invalidate store i =
    getS store i >>= F.mapM_ (\v -> putS store (K.invalidate v))
{-# INLINABLE invalidate #-}

-- | Empty context constant
noContext :: Context
noContext = Map.empty
{-# INLINABLE noContext #-}

-- | Adds the type into the context map.
--
-- Note: Types are their own record and inserting the
-- same type twice will not preserve the old value of
-- that type.
addToContext :: (T.Typeable t) => Context -> t -> Context
addToContext c t = Map.insert (T.typeOf t) (D.toDyn t) c
{-# INLINABLE addToContext #-}

infixl 7 ~+=:

-- | infix version of 'addToContext'
(~+=:) :: (T.Typeable t) => Context -> t -> Context
c ~+=: t = addToContext c t
{-# INLINABLE (~+=:) #-}

contextWrap' :: (T.Typeable k) => k -> Context -> (k -> a) -> a -> a
contextWrap' k c f df =
    case (Map.lookup key c) of
        Nothing -> df
        Just d ->
            let path = D.fromDynamic d
            in case path of
                Nothing -> df
                Just p -> f p
    where
        key = T.typeOf k
-- | Helps unwrap the context value desired.
-- If the value is not present, the default action will be executed.
contextWrap :: (T.Typeable k)
            => Context -- ^ Context anticipated to have feature
            -> (k -> a)
            -- ^ Given a successful context, give this Action / Result
            -> a -- ^ Default Action for failure
            -> a -- ^ Final Action / Result
contextWrap = contextWrap' undefined
