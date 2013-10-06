{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
module Solar.Data.Graph.Direction where

import Data.Typeable
import Data.Generics
import GHC.Generics as G
import Data.Monoid

data KVDirection = In | Out | Both
    deriving (Show, Read, Enum, Bounded, Eq, Ord, Typeable, Data, G.Generic)

instance Monoid KVDirection where
	mempty = Out
	mappend a b
		| a == Both = a
		| b == Both = b
		| a /= b 	= Both
		| otherwise = a