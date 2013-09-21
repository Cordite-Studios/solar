{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solar.Storage.FS where

import Solar.Storage
import Data.Typeable as T
import Data.Dynamic  as D
import Data.Map      as M
import Solar.Data.KV as K
import qualified Data.ByteString.Lazy as BL
import Data.Text as TX
import Control.Applicative((<*>),(<$>), pure)
import Control.Exception(catch, IOException)
import System.Directory(createDirectoryIfMissing)

newtype KFilePath = KFilePath
    { fsPath :: String }
    deriving (Show, Typeable)

data FSMethod n r c d c' = FSMethod
    { fsExt :: String
    , fsRead :: BL.ByteString -> Maybe (KV n r c d c')
    , fsWrite :: KV n r c d c' -> BL.ByteString
    }

getFS :: (Show n)
      => FSMethod n r c d c'
      -> Context
      -> KVIdentifier n
      -> IO (Maybe (KV n r c d c', Context))
getFS fsm c i =
    contextWrap c d n
    where
        n = return Nothing
        d p = do
            let fullPath = toPath (fsPath p) i (fsExt fsm)
            catch (r fullPath) ex
        r p = do
            v <- BL.readFile p
            return $ (,)
                <$> fsRead fsm v
                <*> pure c
        ex :: IOException -> IO (Maybe a)
        ex e = return Nothing

putFS :: (Show n)
      => FSMethod n r c d c'
      -> Context
      -> KV n r c d c'
      -> IO (KV n r c d c', Context)
putFS fsm c kv =
    contextWrap c d n
    where
        n = return (kv, c)
        d p = do
            let path = fsPath p
                i = identifier.meta $ kv
                fullPath = toPath path i (fsExt fsm)
                dirPath = toPath' path i
                v = fsWrite fsm kv
            createDirectoryIfMissing True dirPath
            putStrLn $ "Making dir " ++ dirPath
            BL.writeFile fullPath v
            return (kv, c)

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