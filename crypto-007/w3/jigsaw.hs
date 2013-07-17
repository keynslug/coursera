-- Jigsaw

{-# LANGUAGE BangPatterns #-}

import Data.ByteString.Lazy (ByteString, append, fromStrict)
import Data.ByteString.Internal (createAndTrim)

import System.IO
import System.Environment (getArgs)

import Data.Digest.Pure.SHA

----

getChainHash :: (ByteString -> Digest h) -> Handle -> Int -> IO (Digest h)
getChainHash hash fd bs = do
    sz <- fmap fromIntegral $ hFileSize fd
    let lbo = bs * (sz `div` bs)
        (blk:blks) = map (readBlock bs) $ enumFromThenTo lbo (lbo - bs) 0
    dg <- fmap hash blk
    chain dg blks where
        chain !dg []     = return dg
        chain !dg (b:bs) = b >>= \b' -> chain (hash $ append b' $ bytestringDigest dg) bs
        readBlock bs o   = do
            hSeek fd AbsoluteSeek $ fromIntegral o
            fmap fromStrict $ createAndTrim bs $ \p -> hGetBuf fd p bs

main = do
    fn <- fmap head getArgs
    fd <- openFile fn ReadMode
    getChainHash sha256 fd 1024 >>= print
    hClose fd
