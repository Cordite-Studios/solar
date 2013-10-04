{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Solar.Storage.Context
    ( Context(..) 
    , noContext
    , addToContext
    , (~+=:)
    -- * Context Wrappers
    , contextWrap
    , contextWrap2
    , contextWrap3
    , contextWrap4
    , contextWrap5
    , contextWrap6
    , contextWrap7
    , contextWrapS
    , contextWrapS2
    , contextWrapS3
    , contextWrapS4
    , contextWrapS5
    , contextWrapS6
    , contextWrapS7
    )
where

import           Solar.Storage.Types
import           Control.Monad.Trans.RWS as R
import           Data.Monoid(Monoid(..))

import qualified Data.Dynamic  as D
import qualified Data.Map      as Map
import qualified Data.Typeable as T


-- | Empty context constant
noContext :: Context
noContext = Context Map.empty
{-# INLINABLE noContext #-}

-- | Adds the type into the context IOap.
--
-- Note: Types are their own record and inserting the
-- same type twice will not preserve the old value of
-- that type.
addToContext    :: (T.Typeable t)
                => Context -- ^ The old context
                -> t -- ^ The new value
                -> Context -- ^ The resulting value added to the context

addToContext c t = Context $ Map.insert (T.typeOf t) (D.toDyn t) (unwrapContext c)
{-# INLINABLE addToContext #-}

infixl 7 ~+=:

-- | infix version of 'addToContext'
(~+=:) :: (T.Typeable t) => Context -> t -> Context
c ~+=: t = addToContext c t
{-# INLINABLE (~+=:) #-}

contextWrap'    :: (T.Typeable k)
                => k -- ^ The type of this value
                -> Context -- ^ The context to be searched
                -> (k -> a) -- ^ What to execute if found
                -> a -- ^ Default if not found
                -> a -- ^ Resulting action
contextWrap' k c f df =
    case (Map.lookup key (unwrapContext c)) of
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
contextWrapS :: (T.Typeable k, Monad m, Monoid b)
             => (k -> RWST a b Context m z) -- ^ Action to take
             -> RWST a b Context m z -- ^ Default Action when context resolution fails
             -> RWST a b Context m z -- Resulting Action
contextWrapS action defAction = do
    c <- get
    contextWrap c action defAction


contextWrap2 c f d = contextWrap c (\v -> contextWrap  c (f v) d) d
contextWrap3 c f d = contextWrap c (\v -> contextWrap2 c (f v) d) d
contextWrap4 c f d = contextWrap c (\v -> contextWrap3 c (f v) d) d
contextWrap5 c f d = contextWrap c (\v -> contextWrap4 c (f v) d) d
contextWrap6 c f d = contextWrap c (\v -> contextWrap5 c (f v) d) d
contextWrap7 c f d = contextWrap c (\v -> contextWrap6 c (f v) d) d

contextWrapS2 f d = contextWrapS (\v -> contextWrapS  (f v) d) d
contextWrapS3 f d = contextWrapS (\v -> contextWrapS2 (f v) d) d
contextWrapS4 f d = contextWrapS (\v -> contextWrapS3 (f v) d) d
contextWrapS5 f d = contextWrapS (\v -> contextWrapS4 (f v) d) d
contextWrapS6 f d = contextWrapS (\v -> contextWrapS5 (f v) d) d
contextWrapS7 f d = contextWrapS (\v -> contextWrapS6 (f v) d) d