module Matrix where

import Data.Maybe
import Data.List

type Matrix = [[Double]]
type Coordinates = (Int,Int)

term :: (Int, Int) -> [[Double]]-> Maybe Double
term (x,y) _       | x < 1 || y < 1  = Nothing
term _     []      = Nothing
term (1,y) ([]:ls) = Nothing
term (1,1) (l:ls)  = Just (head l)
term (1,y) (l:ls)  = term (1,y-1) (tail l: ls)
term (x,y) (l:ls)  = term (x-1,y) ls

isSquare :: [[Double]] -> Bool
isSquare [] = False
isSquare ls = all (== length ls) $ map length ls

sarrus :: [[Double]] -> Double
sarrus mtrx = sarrus' (i,i) (i,n) n 0 (map (\ls -> ls ++ (init ls)) mtrx)
    where
    i = 1
    n = length mtrx
    sarrus' :: (Int,Int) -> (Int,Int) -> Int -> Double -> [[Double]] -> Double
    sarrus' (a,b) (c,d) n acc mtrx = if b > n then acc 
        else 
            let pos = posProd (a,b) n 1 mtrx 
                neg = negProd (c,d) n 1 mtrx in
            sarrus' (a,b+1) (c,d+1) n (acc + pos - neg) mtrx

posProd :: (Int,Int) -> Int -> Double -> [[Double]] -> Double
posProd (i,j) n acc mtrx = if i > n then acc 
    else posProd (i+1,j+1) n (fromJust (term (i,j) mtrx) * acc) mtrx

negProd :: (Int,Int) -> Int -> Double -> [[Double]] -> Double
negProd (i,j) n acc mtrx = if i > n then acc 
    else negProd (i+1,j-1) n (fromJust (term (i,j) mtrx) * acc) mtrx

minor :: (Int,Int) -> [[Double]] -> [[Double]]
minor (i,j) = map (removeAt (j-1)) . removeAt (i-1) 
    where
    removeAt :: Int -> [a] -> [a]
    removeAt x ls = take x ls ++ drop (x+1) ls

cofactor :: (Int,Int) -> [[Double]] -> Double
cofactor (i,j) mtrx = ((-1)^(i+j)) * (det . minor (i,j) $ mtrx)

det :: [[Double]] -> Double
det mtrx = case length mtrx of
    1 -> head . head $ mtrx
    2 -> posProd (1,1) 2 1 mtrx - negProd (1,2) 2 1 mtrx
    3 -> sarrus mtrx
    _ -> sum 
        . map (\j -> (*) (head mtrx !! (j-1)) $ cofactor (1,j) mtrx) 
            $ [1..length mtrx] 

