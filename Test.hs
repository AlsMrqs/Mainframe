module Test where

import Prelude hiding (exp)
import System.Random
import Data.Either
import Data.List
import Parser

symbols = "0123456789" ++ "+-" ++ "." ++ "e" ++ "(,)" ++ "xyz"

genIdx :: IO Int
genIdx = newStdGen >>= return . flip mod (length symbols). fst . random 

genNumber :: IO [Char]
genNumber = fmap (map ((!!) symbols)) . sequence . take 7 . cycle $ [genIdx]

runTest :: IO ()
runTest = do
    generation <- (sequence . take 10000 $ cycle [genNumber])
    putStrLn ""
    print . filter (isRight . parser) $ generation

