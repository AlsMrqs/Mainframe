module Machine.Solve where
-- module Trace where

import qualified Control.Monad.State as State
import qualified System.Random as Random
import qualified Data.List as List
import qualified Data.Map as Map
import Numeric.Natural

import qualified Machine.Matrix as Matrix

type Dictionary = Map.Map Char Double

calculate :: Char -> [Double] -> State.StateT Dictionary (Either (Char,Dictionary)) Char
calculate c []      = return c
calculate c (0:xs)  = calculate c xs
calculate c (x:xs)  = do
    coefficients <- State.get
    let independent = State.lift (Left (c,coefficients))
    let diff (y,ks) = (-) y . sum $ zipWith ((*) . snd) (Map.toList coefficients) ks
    if (/=) (length coefficients) ((length xs) - 1) then independent -- [!]
    else 
        case (List.uncons . reverse) xs of
            Nothing     -> independent -- [!]
            Just (y,ks) -> State.put (Map.insert c (diff (y,ks) / x) coefficients) 
                >> return (succ c)

solve :: [[Double]] -> Dictionary -> Either (Char,Dictionary) Dictionary
solve ls dict = either Left (Right . snd) $
    foldr (\l acc -> acc >>= \(c,dict') -> State.runStateT (calculate c l) dict') (Right ('a',dict)) ls

randomPair :: IO (Double,Double)
randomPair = randomCoord >>= \x -> randomCoord >>= \y -> return (roundTo3 x,roundTo3 y)
    where 
    randomCoord = return . fst . Random.randomR (-99e-3,99e-3) =<< Random.newStdGen

randomDegree :: IO Int
randomDegree = return . fst . Random.randomR (1,3) =<< Random.newStdGen

randomConstant :: IO Double
randomConstant = return . roundTo3 . fst . Random.randomR (-99e-3,99e-3) 
    =<< Random.newStdGen

lineGen :: Matrix.Index -> Natural -> (Double,Double) -> Double -> Matrix.Line Double
lineGen idx i (x,y) k = Matrix.Line idx list
    where
    list = map (roundTo3 . (^) x) (List.reverse [1..i]) ++ [k,y]

---------------------{ Assembly System }---------------------

-- todo: intermediary functions (β-ex)

newMatrix :: (Double,Double) -> (Double,Double) -> IO (Matrix.Matrix Double)
newMatrix (xp1,yp1) (xp2,yp2) = do
    k <- randomDegree
    let exponents = List.reverse [0..k]

    (putStrLn . (++) "Exponents: " . show) exponents

    let l1 = map (\expo -> roundTo3 $ (xp1^expo)) exponents 
        l2 = map (\expo -> roundTo3 $ (xp2^expo)) exponents 

    (return . Matrix.fromList) [(++) l1 [yp1],(++) l2 [yp2]]

roundTo3 :: Double -> Double
roundTo3 k = fromIntegral (round (k * 1e3) :: Integer) / 1e3

