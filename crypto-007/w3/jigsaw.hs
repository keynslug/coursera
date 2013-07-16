-- Jigsaw

{-# LANGUAGE BangPatterns #-}

import Data.ByteString.Lazy (ByteString, append, fromStrict)
import Data.ByteString.Internal (createAndTrim)

import System.IO
import System.Environment (getArgs)

import Data.Digest.Pure.SHA

----

getChainHash :: (ByteString -> Digest h) -> Handle -> Integer -> IO (Digest h)
getChainHash hash fd bs = do
    sz <- hFileSize fd
    let lbo = bs * (sz `div` bs)
        (blk:blks) = map (readBlock bsi) $ enumFromThenTo lbo (lbo - bs) 0
    dg <- fmap hash blk
    chain dg blks where
        readBlock bs o    = do
            hSeek fd AbsoluteSeek o
            fmap fromStrict $ createAndTrim bs $ \p -> hGetBuf fd p bs
        bsi              = fromIntegral bs
        chain !dg []     = return dg
        chain !dg (b:bs) = b >>= \b' -> chain (hash $ append b' $ bytestringDigest dg) bs

main = do
    fn <- fmap head getArgs
    fd <- openFile fn ReadMode
    getChainHash sha256 fd 1024 >>= print
    hClose fd
