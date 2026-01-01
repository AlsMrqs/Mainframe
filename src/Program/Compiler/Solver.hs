module Compiler.Solver where

import Engine.Math.Space
import Compiler.Parser
import Compiler.Lexer

solve :: Expression -> Point -> Double
solve tree (x,y,z) = case tree of
    Node n l r -> case (\(Token i _) -> i) n of
        "*" -> (solve l (x,y,z)) * (solve r (x,y,z))
        "/" -> (solve l (x,y,z)) / (solve r (x,y,z))
        "+" -> (solve l (x,y,z)) + (solve r (x,y,z))
        "-" -> (solve l (x,y,z)) - (solve r (x,y,z))
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

