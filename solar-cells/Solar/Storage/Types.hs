{- |
Has the types which are used in this library
-}
module Solar.Storage.Types where

import qualified Data.Typeable as T
import qualified Data.Dynamic  as D
import qualified Data.Map      as Map

-- | Contexts are needed for instances like riak
-- which comes with a vector clock.
-- 
-- It may also be used to store connections or
-- pools.
newtype Context = Context {
        unwrapContext :: Map.Map T.TypeRep D.Dynamic
    }