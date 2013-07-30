{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Moduli (
        Mod,
        ModT,
        withModulus
    ) where

import Data.Proxy
import Data.Reflection

data Mod n a = Mod !a !a

type ModT a = forall n. (Reifies n a) => Mod n a

instance (Eq a) => Eq (Mod n a) where
    Mod _ x == Mod _ y = x == y

instance (Ord a) => Ord (Mod n a) where
    compare (Mod _ x) (Mod _ y) = compare x y

instance (Integral a, Show a) => Show (Mod n a) where
    show (Mod n x) = concat [show x, " (mod ", show n, ")"]

instance (Reifies n a, Integral a, Enum a) => Enum (Mod n a) where
    fromEnum (Mod _ x) = fromEnum x
    toEnum x = Mod n (mod (toEnum x) n)
        where n = reflect (Proxy :: Proxy n)

instance (Reifies n a, Integral a, Real a) => Real (Mod n a) where
    toRational (Mod _ x) = toRational x

instance (Reifies n a, Integral a) => Integral (Mod n a) where
    divMod (Mod n x) (Mod _ y) = (Mod n (div x y), Mod n (mod x y))
    quotRem = divMod
    toInteger (Mod _ x) = toInteger x

instance (Reifies n a, Integral a) => Num (Mod n a) where
    Mod n x + Mod _ y = Mod n (mod (x + y) n)
    Mod n x - Mod _ y = Mod n (mod (n + mod (x - y) n) n)
    Mod n x * Mod _ y = Mod n (mod (x * y) n)
    negate (Mod n x) = Mod n (n - x)
    abs = id
    signum x@(Mod _ 0) = x
    signum (Mod n _) = Mod n 1
    fromInteger x = Mod n (mod (n + mod (fromInteger x) n) n)
        where n = reflect (Proxy :: Proxy n)

instance (Reifies n a, Integral a) => Fractional (Mod n a) where
    fromRational = undefined
    recip (Mod _ 0) = 1 / 0
    recip (Mod n x) = recip' $ gcd' x n where
        recip' (1, k, _) = Mod n (mod (k + n) n)
        recip' _ = undefined
        gcd' a 0 = (a, 1, 0)
        gcd' a b = let
            (q, r) = divMod a b
            (g, s, t) = gcd' b r
            in (g, t, s - q * t)

withModulus :: forall a. a -> (forall n. (Reifies n a) => Mod n a) -> a
withModulus n mx = reify n $ \(_ :: Proxy n) ->
    case mx :: Mod n a of
        Mod _ x -> x
