{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Solar.Data.KV.Cereal (CerealKV(..)) where

import GHC.Generics as G
import Data.Serialize as S
import Solar.Data.KV
import Solar.Data.Graph.Direction
import Solar.Data.Cereal

instance Serialize KVDirection where
instance (Serialize n) => Serialize (KVIdentifier n) where
instance (Serialize n, Serialize r, Serialize c) => Serialize (KVLink n r c) where
instance (Generic (KVMeta a b c), Serialize a, Serialize b, Serialize c) => Serialize (KVMeta a b c) where
instance (Serialize a, Serialize b, Serialize c, Serialize (d a b c), Serialize (e a b c)) => Serialize (KV a b c d e) where
instance Serialize (KVNoCache a b c) where

newtype CerealKV a b c d e = CerealKV { getKV :: (KV a b c d e)} deriving(Generic, Show)
instance (Serialize a, Serialize b, Serialize c, Serialize (d a b c), Serialize (e a b c)) => Serialize (CerealKV a b c d e) where