module Solar.Storage.Context
    ( noContext
    , addToContext
    , (~+=:)
    , contextWrap
    , contextWrapS
    )
where

import Solar.Storage.Types
import Control.Monad.Trans.State as ST

import qualified Data.Typeable as T
import qualified Data.Dynamic  as D
import qualified Data.Map      as Map
import qualified Data.Typeable as T

-- | Empty context constant
noContext :: Context
noContext = Map.empty
{-# INLINABLE noContext #-}

-- | Adds the type into the context IOap.
--
-- Note: Types are their own record and inserting the
-- same type twice will not preserve the old value of
-- that type.
addToContext :: (T.Typeable t) => Context -> t -> Context
addToContext c t = Map.insert (T.typeOf t) (D.toDyn t) c
{-# INLINABLE addToContext #-}

infixl 7 ~+=:

-- | infix version of 'addToContext'
(~+=:) :: (T.Typeable t) => Context -> t -> Context
c ~+=: t = addToContext c t
{-# INLINABLE (~+=:) #-}

contextWrap' :: (T.Typeable k) => k -> Context -> (k -> a) -> a -> a
contextWrap' k c f df =
    case (Map.lookup key c) of
        Nothing -> df
        Just d ->
            let path = D.fromDynamic d
            in case path of
                Nothing -> df
                Just p -> f p
    where
        key = T.typeOf k
-- | Helps unwrap the context value desired.
-- If the value is not present, the default action will be executed.
contextWrap :: (T.Typeable k)
            => Context -- ^ Context anticipated to have feature
            -> (k -> a)
            -- ^ Given a successful context, give this Action / Result
            -> a -- ^ Default Action for failure
            -> a -- ^ Final Action / Result
contextWrap = contextWrap' undefined

-- | Stateful version of 'contextWrap'
contextWrapS :: (T.Typeable k)
             => (k -> StateT Context IO a) -- ^ Action to take
             -> StateT Context IO a -- ^ Default Action when context resolution fails
             -> StateT Context IO a -- Resulting Action
contextWrapS action defAction = do
    c <- get
    contextWrap c action defAction