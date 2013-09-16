module Solar.Data.Graph.Direction where

data KVDirection = In | Out | Both
    deriving (Show, Read, Enum, Bounded, Eq, Ord)