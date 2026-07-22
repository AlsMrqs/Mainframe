module Machine.Solve where

import qualified System.Random as Random
import qualified Data.List as List
import qualified Data.Map as Map
import Numeric.Natural

import qualified Machine.Matrix as Matrix

type Dictionary = Map.Map Char Double

calculate :: Char -> [Double] -> Dictionary -> Either (Char,Dictionary) (Char,Dictionary)
calculate c []     dict = Right (c, dict)
calculate c (0:xs) dict = calculate c xs dict
calculate c (x:xs) dict = case (List.uncons . reverse) xs of
    Nothing     -> Left $ (c, dict)
    Just (y,ks) -> if (==) (length ks) (length dict)
            then Right $ (succ c, Map.insert c (diff (y,ks) / x) dict)
            else Left  $ (c, dict)
    where
    diff (y',ks') = (+) y' . sum $ map negate $ 
        zipWith ((*) . snd) (Map.toList dict) ks'

payload :: [[Double]] -> [[Double]]
payload = List.transpose . dropWhile (all ((==) 0)) . List.transpose

randomPair :: IO (Double,Double)
randomPair = randomCoord >>= \x -> randomCoord >>= \y -> return (roundIt x,roundIt y)
    where 
    randomCoord = fmap (fst . Random.randomR (-99e-2,99e-2)) Random.newStdGen

randomDegree :: IO Int
randomDegree = fmap (fst . Random.randomR (2,3)) Random.newStdGen

randomConstant :: IO Double
randomConstant = fmap (fst . Random.randomR (-90e-2,90e-2)) Random.newStdGen

lineGen :: Matrix.Index -> Natural -> (Double,Double) -> Double -> Matrix.Line Double
lineGen idx i (x,y) k = Matrix.Line idx list
    where
    list = map ((^) x) (List.reverse [1..i]) ++ [k,y]

newMatrix :: Int -> (Double,Double) -> (Double,Double) -> Matrix.Matrix Double
newMatrix k (xp1,yp1) (xp2,yp2) = Matrix.fromList [(++) l1 [yp1],(++) l2 [yp2]]
    where
    exponents = List.reverse [0..k]
    l1 = map (\expo -> (xp1^expo)) exponents 
    l2 = map (\expo -> (xp2^expo)) exponents 

roundIt :: Double -> Double
roundIt k = fromIntegral (round (k * 1e2) :: Integer) / 1e2

roundCof :: Double -> Double
roundCof k = fromIntegral (round (k * 1e5) :: Integer) / 1e5

machine :: Char -> [[Double]] -> Dictionary -> IO Dictionary
machine _ []     dict = return dict
machine c (l:ls) dict = case calculate c l dict of
    Right (k,dict') -> machine k ls dict'
    Left _          -> randomConstant 
        >>= machine (succ c) (l:ls) . flip (Map.insert c) dict

type Coord = (Double,Double)

polish :: Matrix.Matrix Double -> Matrix.Matrix Double
polish = Matrix.elimination . Matrix.fromList . payload . Matrix.toList

adapt :: Matrix.Matrix Double -> [[Double]]
adapt = List.filter ((/=) 1. Matrix.counter) . reverse . Matrix.toList

solution :: Int -> Coord -> Coord -> IO String
solution degree (xp1,yp1) (xp2,yp2) = do
    let baseMatrix = newMatrix degree (xp1,yp1) (xp2,yp2)
        matrix     = polish baseMatrix
    dict <- machine 'a' (adapt matrix) Map.empty
    return . List.intercalate " + "
        -- $ foldr (\(k,u) acc -> ("("++(show u)++") * x^"++(show k)) : acc) [] 
        $ foldr (\(k,u) acc -> ("(("++(fixNegative u)++") * (x^"++(show k)++"))") : acc) [] 
        $ zipWith (flip (,) . snd) (Map.toList dict) [0..degree]

fixNegative :: Double -> String
fixNegative k
    | k >= 0    = show k
    | otherwise = "0"++(show k)

