module Solar.Storage.Types where

import           Solar.Data.KV as K
import qualified Data.Typeable as T
import qualified Data.Dynamic  as D
import qualified Data.Map      as Map

-- | Contexts are needed for instances like riak
-- which comes with a vector clock.
-- 
-- It may also be used to store connections or
-- pools.
type Context = Map.Map T.TypeRep D.Dynamic
newtype TaggedIdentifier n r c d c' = TaggedIdentifier
    { untagIdentifier :: KVIdentifier n
    }