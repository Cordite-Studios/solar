{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{- |
A collection of 'Pipe'-based functions which make querying for
workers easier.
-}
module Solar.Storage.Query
    ( joinRelation
    , getTaggedKV
    , getRelations
    , filterKV
    )
where

import           Pipes
import           Control.Monad.Trans.State as ST
import           Solar.Data.KV
import           Solar.Storage.Types
import           Solar.Storage.Functions
import           Control.Monad(forM_, forever, when)

-- | Given filtering functions, we can use take in a 'KV',
-- iterate over the links and produce 'KV's which satisfy and
-- type check
joinRelation ::  (StorageF n r c d c' m a, StorageF n r c d' c'' m a, ?s :: a, Monad m)
             => (KVLink n r c -> Bool) -- ^ Link filter
             -> (KV n r c d' c'' -> Bool) -- ^ Resolved entity filter
             -> Pipe (KV n r c d c') (KV n r c d' c'') (StateT Context m) ()
joinRelation f f' = getRelations f >-> getTaggedKV >-> filterKV f'

-- | Filters incoming 'KV's according to a boolean function
filterKV :: (StorageF n r c d c' m a, ?s :: a, Monad m)
         => (KV n r c d c' -> Bool) -- ^ Function to filter with
         -> Pipe (KV n r c d c') (KV n r c d c') (StateT Context m) ()
filterKV f = forever $ do
    kv <- await
    when (f kv) $ yield kv

-- | Using a 'TaggedIdentifier', this will produce 'KV's that typecheck
getTaggedKV ::  (StorageF n r c d c' m a, ?s :: a, Monad m)
            => Pipe (TaggedIdentifier n r c (d n r c) (c' n r c)) (KV n r c d c') (StateT Context m) ()
getTaggedKV = forever $ do
    i' <- await
    let i = untagIdentifier i'
    kv' <- lift $ getSF i
    case kv' of
        Nothing -> return ()
        Just kv -> yield kv 

-- | Given a 'KV', it will produce 'TaggedIdentifier's for the
-- entities that are of interest.
getRelations ::  (StorageF n r c d c' m a, ?s :: a, Monad m)
              => (KVLink n r c -> Bool) -- ^ Link Filter
              -> Pipe (KV n r c d c') (TaggedIdentifier n r c d' c'') (StateT Context m) ()
getRelations f = forever $ do
    kv <- await
    let links = relations.meta $ kv
    forM_ links $ \link ->
        when (f link) $ yield $ TaggedIdentifier $ linkIdentifier link
    