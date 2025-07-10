module Fraction where

import Data.List
import Prelude hiding (gcd)

data Fraction = Fraction 
    { numerator :: Int 
    , denominator :: Int } deriving (Eq)

instance Show Fraction where
    show s = show (numerator s) ++ "/" ++ show (denominator s)

solve :: Fraction -> Double
solve frac = (fromIntegral $ numerator frac) / (fromIntegral $ denominator frac)

gcd :: Int -> Int -> Int
gcd x y = if x == 0 || y == 0 then x + y else
    let n = dividers x
        m = dividers y in
    maximum $ intersect n m

dividers :: Int -> [Int]
dividers x = if (abs x) < 2 then [abs x]
    else filter ((==) 0 . mod x) [(abs x),(abs x)-1..1]

simplify :: Fraction -> Fraction
simplify frac = if gcd' <= 1 then frac else Fraction (div num gcd') (div den gcd')
    where
    num  = numerator   frac
    den  = denominator frac
    gcd' = gcd num den

