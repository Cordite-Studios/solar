{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Solar.Data.KV.KV where


import Data.Typeable
import Data.Generics as D
import GHC.Generics as G
import Solar.Data.KV.Meta


data KV namespace relations classes datas cache = KV
    { meta          :: !(KVMeta namespace relations classes)
    -- ^ Meta data for this entity
    , content       :: !(datas namespace relations classes)
    -- ^ Content for this entity
    , caches        :: !(Maybe (cache namespace relations classes))
    -- ^ Caches, optional, use 'kvNoCache' function or 'KVNoCache'
    -- data type if you don't plan for this entity to ever have
    -- caches.
    } deriving (Show, Read, Data, G.Generic)

invalidate :: KV n r c d c' -> KV n r c d c'
invalidate kv = kv { meta = (meta kv) {invalid = True} }

instance (Typeable n, Typeable r, Typeable c, Typeable3 d, Typeable3 c') =>
    Typeable (KV n r c d c') where
    typeOf _ =
        mkTyConApp (mkTyCon3 "solar-spot" "Solar.Data.KV.KV" "KV") []
        `mkAppTy` typeOf (undefined :: n)
        `mkAppTy` typeOf (undefined :: r)
        `mkAppTy` typeOf (undefined :: c)
        `mkAppTy` typeOf (undefined :: d n r c)
        `mkAppTy` typeOf (undefined :: c' n r c)  