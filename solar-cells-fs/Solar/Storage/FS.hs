{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImplicitParams #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{- |
This gives a simple API for file based access,
given that an implict variable holds a structure
that can be converted to a 'FSSettingsD'. 
-}
module Solar.Storage.FS
    ( -- * Data and such
      FSFilePath(..)
    , FSSettingsD(..)
    , FSSettings(..)
    -- * Manipulators
    , getFS
    , putFS
    , delFS
    )
where

import Solar.Storage.Context
import Solar.Storage.Types
import Data.Typeable as T
import Data.Dynamic  as D
import Data.Map      as M
import Solar.Data.KV as K
import qualified Data.ByteString.Lazy as BL
import Data.Text as TX
import Control.Applicative((<*>),(<$>), pure)
import Control.Exception(catch, IOException)
import System.Directory(createDirectoryIfMissing, removeFile)

-- | Holds the file path, should be placed in the context
newtype FSFilePath = FSFilePath
    { fsPath :: String }
    deriving (Show, Read, Typeable)

-- | Data structure with functions for each type of 'KV'
data FSSettingsD n r c d c' = FSSettingsD
    { fsExt :: String
    , fsRead :: BL.ByteString -> Maybe (KV n r c d c')
    , fsWrite :: KV n r c d c' -> BL.ByteString
    }

-- | Type class that a larger datastructure must impliment
-- in order to be used by 'getFS', 'putFS', and 'delFS'
class FSSettings n r c d c' a where
    convertFS :: a -> FSSettingsD n r c d c'


-- | Gets the entity from disk.
-- Will return Nothing if it does not parse.
getFS :: (Show n, FSSettings n r c d c' a, ?s :: a)
      => Context
      -> KVIdentifier n
      -> IO (Maybe (KV n r c d c'), Context)
getFS c i =
    contextWrap c d n
    where
        s = convertFS ?s
        n = return (Nothing, c)
        d p = do
            let fp = fsPath p
                ext = fsExt s
                fullPath = toPath fp i ext
            catch (do
                v <- BL.readFile fullPath
                return (fsRead s v, c)
                ) ex
        ex :: IOException -> IO (Maybe z, Context)
        ex e = return (Nothing, c)

-- | Saves the 'KV' to disk
putFS :: (Show n, FSSettings n r c d c' a, ?s :: a)
      => Context
      -> KV n r c d c'
      -> IO (KV n r c d c', Context)
putFS c kv =
    contextWrap c d n
    where
        n = return (kv, c)
        s = convertFS ?s
        d p = do
            let path = fsPath p
                e = fsExt s
                i = identifier.meta $ kv
                fullPath = toPath path i e
                dirPath = toPath' path i
                v = fsWrite s kv
            createDirectoryIfMissing True dirPath
            BL.writeFile fullPath v
            return (kv, c)

-- | Deletes the 'KV' from disk. Exact type is not needed,
-- but is encouraged
delFS   :: (Show n, FSSettings n r c d c' a, ?s :: a)
        => Context
        -> TaggedIdentifier n r c (d n r c) (c' n r c)
        -> IO (Bool, Context)
delFS = delFS' (convertFS ?s)

delFS'  :: (Show n)
        => FSSettingsD n r c d c'
        -> Context
        -> TaggedIdentifier n r c (d n r c) (c' n r c)
        -> IO (Bool, Context)
delFS' s c i' =
    contextWrap c d n
    where
        n = return (False, c)
        i = untagIdentifier i'
        d p = do
            let path = fsPath p
                e = fsExt s
                fullPath = toPath path i e
            catch (do
                removeFile fullPath
                return (True, c)
                ) ex
        ex :: IOException -> IO (Bool, Context)
        ex _ = n

toPath' :: (Show n) => String -> KVIdentifier n -> String
toPath' path ident =
    path ++ "/" ++ ns
    where
        ns = show $ namespace $ ident
toPath :: (Show n) => String -> KVIdentifier n -> String -> String
toPath path ident ext =
    start ++ "/" ++ k ++ "." ++ ext
    where
        start = toPath' path ident
        k = TX.unpack $ key $ ident
