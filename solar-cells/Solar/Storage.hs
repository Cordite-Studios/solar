{- |
Main entry point to the solar cells library,
re-exports the main functionality of this library.
-}
module Solar.Storage
    (
    -- * Data Structure
      StorageFC(..)
    -- * Stateful Manipulators
    , getSF
    , putSF
    , delSF
    -- * Context management
    , noContext
    , addToContext
    , (~+=:)
    -- * Runners
    , runS
    , runS'
    , runW
    , runW'
    , runU
    -- * Re-exports
    , R.tell
    )
where



import Solar.Storage.Functions
import Solar.Storage.Runners
import Solar.Storage.Class
import Solar.Storage.Context

import           Control.Monad.Trans.RWS as R (tell)