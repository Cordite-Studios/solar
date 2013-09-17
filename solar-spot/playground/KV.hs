{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module KV where

import qualified Solar.Data.KV as K
import System.IO.Unsafe(unsafePerformIO)
import Data.Time.Clock
import Data.Text(pack)
import Data.Typeable
import Data.Serialize as S
import GHC.Generics as G
import Solar.Data.KV.Cereal

data Color = Red | Green | Blue | Yellow
    deriving (Show, Read, Typeable, Generic)
data Vehicle = Car | Truck | Semi | Van
    deriving (Show, Read, Typeable, Generic)
data Ponies = Fluttershy | TwilightSparkle | Rarity | Applejack | PinkiePie | RainbowDash
    deriving (Show, Read, Typeable, Generic)

data Forum n r c = Forum r
    deriving (Show, Typeable, Generic)

instance Serialize Ponies where
instance Serialize Vehicle where
instance Serialize Color where
instance (Serialize r) => Serialize (Forum n r c) where


time = unsafePerformIO $ getCurrentTime

ident = K.KVIdentifier Red (pack "Hai")
ident2 = K.KVIdentifier Blue (pack "Potatoes")
arel = K.KVLink ident2 [Car, Van] [Rarity] K.In time False
met = K.KVMeta ident [RainbowDash, Fluttershy] [arel] time time time False
kv = K.KV met (Forum Truck) K.kvNoCache
