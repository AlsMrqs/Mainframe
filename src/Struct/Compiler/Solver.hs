module Struct.Compiler.Solver where

import Struct.Math -- .Space
import Struct.Compiler.Parser
import Struct.Compiler.Lexer
import Struct.Compiler.Language.Grammar

solve :: [Char] -> Point -> Either String Double
solve str vars = fmap (flip solve' vars) (parse str)

-- insert Math opertors!!!!!!!
solve' :: Tree Token -> Point -> Double -- Point : (input variable) for a cartesian scanner
solve' tree (x,y,z) = case tree of
    Node n l r -> case (\(Token i _) -> i) n of
        "*" -> (solve' l (x,y,z)) * (solve' r (x,y,z))
        "/" -> (solve' l (x,y,z)) / (solve' r (x,y,z))
        "+" -> (solve' l (x,y,z)) + (solve' r (x,y,z))
        "-" -> (solve' l (x,y,z)) - (solve' r (x,y,z))
        "^" -> (solve' l (x,y,z)) ** (solve' r (x,y,z)) -- fl4g{wr0ng}
    Leaf (Token i t) -> case t of
        Variable_ -> case i of
            "x" -> x
            "X" -> x
            "y" -> y
            "Y" -> y
            "z" -> z
            "Z" -> z
        _         -> (Prelude.read i :: Double)
    Empty -> 0

assign :: (Point -> Double, Point -> Double, Point -> Double) -> Point -> Point
assign (f,g,h) pnt = (f pnt, g pnt, h pnt)

