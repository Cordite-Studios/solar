{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module KV where

import qualified Solar.Data.KV as K
import System.IO.Unsafe(unsafePerformIO)
import Data.Time.Clock
import Data.Text(pack)
import Data.Typeable
import Data.Serialize as S
import GHC.Generics as G
import Solar.Data.KV.Cereal
import Solar.Storage.FS as FS
import Solar.Storage as St
import Data.Map as Map
import qualified Data.ByteString.Lazy.UTF8 as U
import Text.Read(readMaybe)
import qualified Data.ByteString.Lazy as BL

data Color = Red | Green | Blue | Yellow
    deriving (Show, Read, Typeable, Generic)
data Vehicle = Car | Truck | Semi | Van
    deriving (Show, Read, Typeable, Generic)
data Ponies = Fluttershy | TwilightSparkle | Rarity | Applejack | PinkiePie | RainbowDash
    deriving (Show, Read, Typeable, Generic)

data Forum n r c = Forum r
    deriving (Show, Typeable, Generic, Read)

instance Serialize Ponies where
instance Serialize Vehicle where
instance Serialize Color where
instance (Serialize r) => Serialize (Forum n r c) where


time = unsafePerformIO $ getCurrentTime

ident = K.KVIdentifier Red (pack "Hai")
ident2 = K.KVIdentifier Blue (pack "Potatoes")
arel = K.KVLink ident2 [Car, Van] [Rarity] K.In time False
met = K.KVMeta ident [RainbowDash, Fluttershy] [arel] time time time False

type TestKV = K.KV Color Vehicle Ponies Forum K.KVNoCache

kv :: TestKV
kv = K.KV met (Forum Truck) K.kvNoCache

kvToBS :: (Show n, Show r, Show c, Show (d n r c), Show (c' n r c))
       => K.KV n r c d c'
       -> BL.ByteString
kvToBS kv =
    U.fromString $ show kv

kvFromBS :: (Read n, Read r, Read c, Read (d n r c), Read (c' n r c))
         => BL.ByteString
         -> Maybe (K.KV n r c d c')
kvFromBS b = readMaybe $ U.toString $ b

fs = FSMethod
    { fsExt = "txt"
    , fsRead = kvFromBS
    , fsWrite = kvToBS
    }

path = KFilePath "/tmp"
cont = noContext ~+=: path
