{-# LANGUAGE OverloadedStrings #-}
module Solar.Storage.Fingerprint where

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Typeable as TP

makeIndexKey :: (Show k) => k -> TP.TypeRep -> T.Text
makeIndexKey k t = f
	where
		sk = T.pack.show $ k
		tr = T.pack.show $ t
		trb = E.encodeUtf8 tr
		trh = MD5.hash trb
		tre = B64.encode trh
		trt = B.take 8 tre
		trd = E.decodeUtf8 trt
		f = T.concat [sk, "-", trd]