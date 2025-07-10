module Test where

import Prelude hiding (exp)
import System.Random
import Data.Either
import Data.List
import Data.Bool
import Parser

--symbols = "0123456789" ++ "+-" ++ "." ++ "e" ++ "xyz" ++ "(,)"
symbols = "0123" ++ "+-" ++ "xyz" -- ++ "(,)"

genIdx :: IO Int
genIdx = newStdGen >>= return . flip mod (length symbols). fst . random 

genNumber :: IO [Char]
genNumber = fmap (map ((!!) symbols)) . sequence . take 5 . cycle $ [genIdx]

model = [return "(", genNumber, return ",", genNumber, return ",", genNumber, return ")"]

test :: Integer -> IO ()
test counter = do
    token <- fmap concat $ sequence model
    if isRight . parser $ token
        then print token
        else return ()

    if counter >= 10000
        then return ()
        else test (counter + 1) 

runTest :: IO ()
runTest = do
    test 0
    putStrLn "Test (done!)"
    input <- getLine
    if input == "exit" then return ()
        else runTest 

