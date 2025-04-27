module Main where

import Prelude hiding (read)
import Data.Maybe
import Data.List
import Graph
import Automaton
import Test

main :: IO ()
main = do
    generation <- (sequence . take 80 $ cycle [genNumber])

    (\(a,b) -> mapM_ (putStrLn . show) [a,b]) 
        . partition ((/=) "" . fst) 
        . map lexer $ generation

    putStrLn ""
    print generation

lexer :: [Char] -> ([Char], [Char])
lexer [] = ([], [])
lexer l@(x:xs) 
    | isNothing automaton = ([], l) 
    | otherwise = read l $ fromJust automaton
    where
        automaton = start x

