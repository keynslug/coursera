---- Nearbies

{-# LANGUAGE BangPatterns #-}

import Data.Bits
import Data.Either
import Data.List (sort)
import Control.Exception (assert)
import System.Environment (getArgs)

type Modulus = Integer

isqrt :: Integer -> Integer
isqrt x = closest (f x) x' where
    x' = 16 ^ (msb x + 1)
    msb = length . takeWhile (/= 0) . iterate (`shiftR` 8)
    f x !s = (s + x `div` s) `div` 2
    closest g s
        | t >= s    = s
        | otherwise = closest g t where
            t = g s

isqrt' :: Integer -> Maybe Integer
isqrt' x
    | xsq ^ 2 == x = Just xsq
    | otherwise    = Nothing where
        xsq = isqrt x

factors :: Modulus -> Integer -> Maybe (Integer, Integer)
factors n i = assert (i >= 0) $ let nsq = 1 + isqrt n in fact n [nsq .. (nsq + i)] where
    fact n [] = Nothing
    fact n (a:as) = maybe (fact n as) (\x -> Just (a - x, a + x)) $ isqrt' (a * a - n)

main = getArgs >>= factorize . parseArgs where
    prints = mapM_ print
    parseArgs :: [String] -> [Either Integer (Char, Integer)]
    parseArgs (('-':c:ss):as) = Right (c, read ss) : parseArgs as
    parseArgs as = map (Left . read) as
    factorize args = do
        let (largs, rargs) = partitionEithers args
            sfs@[pfac, qfac] = map (\c -> maybe 1 id $ lookup c rargs) "pq"
            [ival, n] = lefts args
            fs = factors (pfac * qfac * n) (2 ^ ival)
        case fs of
            Nothing -> print fs
            Just (p', q') -> let f xs = prints . sort . zipWith div xs
                in if p' `mod` pfac == 0 then f [p', q'] sfs else f [q', p'] sfs
