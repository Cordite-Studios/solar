module KV where

import qualified Solar.Data.KV as K
import System.IO.Unsafe(unsafePerformIO)
import Data.Time.Clock
import Data.Text(pack)

data Color = Red | Green | Blue | Yellow
    deriving (Show, Read)
data Vehicle = Car | Truck | Semi | Van
    deriving (Show, Read)
data Ponies = FlutterShy | TwilightSparkle | Rarity | AppleJack | PinkiePie | RainbowDash
    deriving (Show, Read)

data Forum n r c = Forum r
    deriving (Show)
data ForumCache n r c = EmptyForumCache
    deriving (Show)

time = unsafePerformIO $ getCurrentTime

ident = K.KVIdentifier Red (pack "Hai")
ident2 = K.KVIdentifier Blue (pack "Potatoes")
arel = K.KVLink ident2 [Car, Van] [Rarity]
met = K.KVMeta ident [RainbowDash, FlutterShy] [arel] time False