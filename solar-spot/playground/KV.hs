{-# LANGUAGE DeriveDataTypeable #-}
module KV where

import qualified Solar.Data.KV as K
import System.IO.Unsafe(unsafePerformIO)
import Data.Time.Clock
import Data.Text(pack)
import Data.Typeable

data Color = Red | Green | Blue | Yellow
    deriving (Show, Read, Typeable)
data Vehicle = Car | Truck | Semi | Van
    deriving (Show, Read, Typeable)
data Ponies = FlutterShy | TwilightSparkle | Rarity | AppleJack | PinkiePie | RainbowDash
    deriving (Show, Read, Typeable)

data Forum n r c = Forum r
    deriving (Show, Typeable)

time = unsafePerformIO $ getCurrentTime

ident = K.KVIdentifier Red (pack "Hai")
ident2 = K.KVIdentifier Blue (pack "Potatoes")
arel = K.KVLink ident2 [Car, Van] [Rarity] K.In time False
met = K.KVMeta ident [RainbowDash, FlutterShy] [arel] time time time False
kv = K.KV met (Forum Truck) K.kvNoCache