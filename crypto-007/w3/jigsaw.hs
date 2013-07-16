-- Jigsaw

{-# LANGUAGE BangPatterns #-}

import Data.Char
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import System.IO
import System.Environment (getArgs)

import Data.Digest.Pure.SHA

----

getChainHash :: (ByteString -> Digest h) -> Integer -> ByteString -> Digest h
getChainHash hash block bs
    | len > bl  = chain $ ByteString.splitAt bl bs
    | otherwise = hash bs where
        bl             = fromIntegral block
        len            = ByteString.length bs
        chain (b, bs') = hash $ ByteString.append b $ hashBytes h where
            !h = getChainHash hash block bs'

hashBytes :: Digest h -> ByteString
hashBytes = bytestringDigest

----

main = getArgs >>= ByteString.readFile . head >>= print . getHash where
    getHash = getChainHash sha256 1024
