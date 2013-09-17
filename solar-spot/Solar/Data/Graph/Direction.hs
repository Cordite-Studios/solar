{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
module Solar.Data.Graph.Direction where

import Data.Typeable
import Data.Generics
import GHC.Generics as G

data KVDirection = In | Out | Both
    deriving (Show, Read, Enum, Bounded, Eq, Ord, Typeable, Data, G.Generic)