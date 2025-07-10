module Matrix where

import Data.Maybe
import Data.List
import Data.Bool
import Data.Vector
-- import Wheel

term :: Num a => (Int, Int) -> [[a]]-> Maybe a
term (x,y) _       | x < 1 || y < 1  = Nothing
term _     []      = Nothing
term (1,y) ([]:ls) = Nothing
term (1,1) (l:ls)  = Just (head l)
term (1,y) (l:ls)  = term (1,y-1) (tail l: ls)
term (x,y) (l:ls)  = term (x-1,y) ls

isSquare :: Num a => [[a]] -> Bool
isSquare [] = False
isSquare ls = all (== length ls) $ map length ls

isSingular :: (Eq a, Num a) => [[a]] -> Bool
isSingular = (==) 0 . det

posProd :: Num a => (Int,Int) -> Int -> a -> [[a]] -> a
posProd (i,j) n acc mtrx = if i > n then acc else 
    posProd (i+1,j+1) n (fromJust (term (i,j) mtrx) * acc) mtrx

negProd :: Num a => (Int,Int) -> Int -> a -> [[a]] -> a
negProd (i,j) n acc mtrx = if i > n then acc else 
    negProd (i+1,j-1) n (fromJust (term (i,j) mtrx) * acc) mtrx

minor :: Num a => (Int,Int) -> [[a]] -> [[a]]
minor (i,j) = map (removeAt (j-1)) . removeAt (i-1) 
    where
    removeAt :: Int -> [a] -> [a]
    removeAt x ls = take x ls ++ drop (x+1) ls

cof :: Num a => (Int,Int) -> [[a]] -> a
cof (i,j) = (*) ((-1)^(i+j)) . det . minor (i,j)

det :: Num a => [[a]] -> a
det mtrx = case length mtrx of
    1 -> head . head $ mtrx
    2 -> posProd (1,1) 2 1 mtrx - negProd (1,2) 2 1 mtrx
    _ -> sum 
        . map (\j -> (*) (head mtrx !! (j-1)) $ cof (1,j) mtrx) 
            $ [1..length mtrx] 

identity :: Num a => [[a]] -> [[a]]
identity mtrx = map line [0..(length mtrx)-1]
    where
    line :: Num a => Int -> [a]
    line idx = take (length mtrx) $ replicate idx 0 ++ [1] ++ cycle [0]

divide :: Fractional a => [[a]] -> a -> [[a]]
divide []     _ = []
divide (x:xs) y = map (/y) x : divide xs y

inverse :: Fractional a => [[a]] -> [[a]]
inverse mtrx = let det' = det mtrx in map (map (/det')) $ cofactor (transpose mtrx)

cofactor :: Num a => [[a]] -> [[a]]
cofactor mtrx = aux 1 (uncons mtrx)
    where
    aux i idx = case idx of
        Nothing     -> []
        Just (x,xs) -> [cof (i,j) mtrx | j <- [1..length x]] : aux (i+1) (uncons xs)

prod :: Num a => [[a]] -> [[a]] -> [[a]]
prod m1 m2 = aux (uncons m1) $ transpose m2
    where
    aux ref mtrx = case ref of
        Nothing     -> []
        Just (x,xs) -> map (\ln -> sum $ zipWith (*) x ln) mtrx : aux (uncons xs) mtrx

