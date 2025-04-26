module Test where

import System.Random
import Prelude hiding (exp)

numbers = "0123456789" ++ "+-" ++ "." ++ "e"

genIdx :: IO Int
genIdx = newStdGen >>= return . flip mod 14 . fst . random 

genNumber :: IO [Char]
genNumber = fmap (map ((!!) numbers)) . sequence . take 12 . cycle $ [genIdx]

