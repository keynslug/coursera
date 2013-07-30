{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

import System.Environment (getArgs)
import qualified Data.Map.Strict as Map

import Moduli

type Modulus = Integer
type ModComp = Integer -> ModT Integer

inverseModComps :: Modulus -> ModComp -> [(Integer, Integer)]
inverseModComps p comp = map compPair [0..] where
    compPair i = let !x = withModulus p (comp i) in (x, i)

discreteLog :: Modulus -> Integer -> Integer -> Int -> Maybe Integer
discreteLog p !g !h bits = let
    gb = withModulus p $ (fromInteger g) ^ b0
    comps = inverseModComps p
    upper = take (fromInteger b1) $ comps (\i -> (fromInteger gb) ^ i)
    lower = take (fromInteger b0) $ comps (\i -> (fromInteger h) / ((fromInteger g) ^ i))
    !upperMap = Map.fromList upper
    in collide upperMap lower where
        (hbits, bitsmod) = bits `divMod` 2
        b0 = 2 ^ (hbits + bitsmod)
        b1 = 2 ^ hbits
        collide _ [] = Nothing
        collide m ((x, i0):xs) = case Map.lookup x m of
            Just i1 -> Just (i0 + i1 * b0)
            Nothing -> collide m xs

main :: IO ()
main = do
    args <- getArgs
    let [bits, p, g, h] = map read args
    print $ discreteLog p g h (fromInteger bits)
