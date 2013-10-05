{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Provides a "default"-like implimentation that uses only
the context to provide functionality.

Use '()' as the 'StorageFC'
-}
module Solar.Storage.Unit where

import Solar.Storage.Class
import Solar.Storage.Context
import Data.Map as M
import Solar.Data.KV
import Data.Typeable


instance (Monad m, Typeable n, Typeable r, Typeable c, Typeable3 d, Typeable3 c', Ord n)
    => StorageFC n r c d c' m () where
    sfcGet () c i =
        contextWrap c d f
        where
            f = return (Nothing, c)
            d   :: Map (KVIdentifier n) (KV n r c d c')
                -> m (Maybe (KV n r c d c'), Context)
            d m = return (M.lookup i m, c)

    sfcPut () c kv =
        contextWrap c d f
        where
            i = identifier.meta $ kv
            f = d M.empty
            -- ↓ Put into the map
            d m = return (kv, c ~+=: (M.insert i kv m))

    sfcDel () c i' = do
        contextWrap c d f
        where
            i = untagIdentifier i'
            f = return (False, c)
            d   :: Map (KVIdentifier n) (KV n r c d c')
                -> m (Bool, Context)
            d m = case M.lookup i m of
                Nothing -> f
                Just _ -> return (True, c ~+=: (M.delete i m))