---- Dustify

import Data.Bits
import Data.Char
import System.Environment (getArgs)

import Moduli

dustify :: (Integer, Integer) -> Integer -> Integer -> Integer
dustify (p, q) e sub = withModulus n $ (fromInteger sub) ^ d where
    n = p * q
    phi = (p - 1) * (q - 1)
    d = withModulus phi $ recip (fromInteger e)

main = do
    [p, q, e, s] <- fmap (map read) getArgs
    print . map chr . tail . dropWhile (/= 0) . bytes [] $ dustify (p, q) e s where
        bytes bs 0 = bs
        bytes bs i = bytes (fromInteger (i .&. 0xff) : bs) (i `shiftR` 8)
