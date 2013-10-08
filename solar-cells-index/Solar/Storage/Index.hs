{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{- |
Holds the basic types for a built in indexing feature, built on
the primitives that the user implements.
-}
module Solar.Storage.Index where

import           Data.Map as M
import           GHC.Generics as G
import           Solar.Data.KV as K
import           Data.Typeable
import           Data.Set as S
import qualified Data.Text as T
import           Data.Monoid
import           Solar.Storage
import           Solar.Storage.Context
import           Control.Monad.Trans.RWS as R
import           Control.Monad.Trans.Class(lift)
import           Solar.Storage.Fingerprint
import           Control.Monad(forM_)
import           Data.Foldable(foldrM)


newtype TaggedMap n r c d c' v = TaggedMap
    { untagMap :: M.Map v (S.Set (KVIdentifier n))
    } deriving (Show, Read, Typeable, G.Generic, Eq, Ord)

-- | Structure for a single generic index entry
data KVIndex d c' k v n r c = KVIndex
    { indexName :: k -- ^ Name of this index
    , indexValues :: TaggedMap n r c (d n r c) (c' n r c) v
    -- ^ Simple map of the index values 
    } deriving (Show, Read, G.Generic, Eq, Ord)

instance (Typeable3 d, Typeable3 c', Typeable k, Typeable v, Typeable n, Typeable r, Typeable c)
    => Typeable (KVIndex d c' k v n r c) where
    typeOf _ =
        mkTyConApp (mkTyCon3 "solar-cells-index" "Solar.Storage.Index" "KVIndex") []
        `mkAppTy` typeOf (undefined :: d n r c)
        `mkAppTy` typeOf (undefined :: c' n r c)
        `mkAppTy` typeOf (undefined :: k)
        `mkAppTy` typeOf (undefined :: v)
        `mkAppTy` typeOf (undefined :: n)
        `mkAppTy` typeOf (undefined :: r)
        `mkAppTy` typeOf (undefined :: c)



-- | Synonym structure where we include 'KVNoCache'
type KVIndex' n r c d c' k v= KV n r c (KVIndex d c' k v) KVNoCache
-- | Simple class to get the namespace from what the user is using
class IndexNamespace a where
    -- | Constant get function to be used when accessing indexes.
    getIxNS :: a



class (Typeable3 d, Typeable3 c', Typeable n, Typeable r, Typeable c, Ord n, Ord r, Ord c, Monoid n) => StorageIndexable n r c d c' k v where
    -- | Adds an index to the context for an identifier
    sfcAddIndex :: (IndexNamespace n, StorageFC n r c (KVIndex d c' k v) KVNoCache m a, Ord n, Ord k, Ord v, Show k)
                => a -- ^ The current data structure in question
                -> Context -- ^ The Context of the operation
                -> KV n r c d c'
                -- ^ The entity to apply an index to
                -> k -- ^ The index name
                -> v -- ^ The index value
                -> m (Context)
                -- ^ The monadic action to take where the index
                -- value is applied 
    sfcAddIndex s c kv' k v = do
        let ky = (makeIndexKey k (typeOf (undefined :: KV n r c d c')))
            i = KVIdentifier (getIxNS :: n) ky
            kvi = identifier.meta $ kv'
        (v', c1) <- sfcGet s c i
        case v' of
            Nothing -> do
                let mp = TaggedMap $ M.insert v (S.singleton kvi) M.empty
                    mp :: TaggedMap n r c (d n r c) (c' n r c) v
                    kv = KV (mempty {identifier = i}) (KVIndex k mp) kvNoCache
                    kv :: KVIndex' n r c d c' k v
                (_, c2) <- sfcPut s c1 kv
                return c2
            Just (kv :: KVIndex' n r c d c' k v) -> do
                let tm = indexValues.content $ kv
                    tm :: TaggedMap n r c (d n r c) (c' n r c) v
                    om = untagMap tm

                case M.lookup v om of
                    Nothing -> do
                        let nm = M.insert v (S.singleton kvi) om
                            nkv = kv { content = (content kv) { indexValues = TaggedMap nm}}
                            nkv :: KVIndex' n r c d c' k v
                        (_, c2) <- sfcPut s c1 nkv
                        return c2
                    Just os -> do
                        let nm = M.insert v (S.insert kvi os) om
                            nkv = kv { content = (content kv) { indexValues = TaggedMap nm}}
                            nkv :: KVIndex' n r c d c' k v
                        (_, c2) <- sfcPut s c1 nkv
                        return c2

    sfcGetByIndex   :: (IndexNamespace n,StorageFC n r c (KVIndex d c' k v) KVNoCache m a, Ord n, Show k, Ord v)
                    => a
                    -> Context
                    -> k
                    -> v
                    -> m ([TaggedIdentifier n r c (d n r c) (c' n r c)], Context)
    sfcGetByIndex s c k v = do
        let ky = (makeIndexKey k (typeOf (undefined :: KV n r c d c')))
            i = KVIdentifier (getIxNS :: n) ky
        (v', c1) <- sfcGet s c i
        case v' of
            Nothing -> return ([], c1)
            Just (kv :: KVIndex' n r c d c' k v) -> do
                let tm = indexValues.content $ kv
                    om = untagMap tm
                case M.lookup v om of
                    Nothing -> return ([], c1)
                    Just os -> do
                        let ls = S.toList os
                        return (Prelude.map tagIt ls, c1)
        where
            tagIt :: KVIdentifier n -> TaggedIdentifier n r c (d n r c) (c' n r c)
            tagIt i = TaggedIdentifier i

-- | Adds indexing features to the 'KV' kinds
class (Ord k, Ord v, Show k) => KVIndexable n r c d c' k v | k -> v where
    -- | Provides the result pairs for index values
    indexResults :: KV n r c d c' -> [(k, v)]
    indexAdd    :: (StorageFC n r c (KVIndex d c' k v) KVNoCache m a, StorageIndexable n r c d c' k v, IndexNamespace n, Ord n, Monoid n, Ord c)
                => a
                -> Context
                -> k
                -> KV n r c d c'
                -> m (Context)
    indexAdd a c _ kv = do
        let d = indexResults kv
            d :: [(k, v)]
        foldrM f c d
        where
            f (k, v) c = sfcAddIndex a c kv k v

-- | A typed template for running indexes. These ought to be made
-- for their own key types, like saying
-- @indexMyType = indexSF' (undefined :: MyType)@
indexSF'    :: (StorageFC n r c d c' m a, StorageFC n r c (KVIndex d c' k v) KVNoCache m a, KVIndexable n r c d c' k v, IndexNamespace n, Monoid b, Ord n, StorageIndexable n r c d c' k v, Monoid n, Ord c)
            => k
            -> KV n r c d c'
            -> RWST a b Context m ()
indexSF' k kv = do
    c <- get
    s <- ask
    c' <- lift $ indexAdd s c k kv
    put c'
