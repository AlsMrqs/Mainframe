module Math.Solver where

import qualified Math.AST as Math.AST
import qualified Data.Map as Map

varError :: String
varError = "Variable not found!"

type Expression = Math.AST.Expression

binOp :: Map.Map [Char] Double -> (Double -> Double -> Double) -> 
    Math.AST.Expression -> Math.AST.Expression -> Either [Char] Double
binOp dict op a b = solve dict a
    >>= \l -> solve dict b
    >>= \r -> return (op l r)

solve :: Map.Map [Char] Double -> Math.AST.Expression -> Either [Char] Double
solve dict expr = case expr of
    (Math.AST.Variable str) -> maybe (Left varError) return (Map.lookup str dict)
    (Math.AST.Value x)      -> return x
    (Math.AST.Multiplication a b) -> binOp dict (*) a b
    (Math.AST.Division a b)       -> binOp dict (/) a b
    (Math.AST.Subtraction a b)    -> binOp dict (-) a b
    (Math.AST.Addition a b)       -> binOp dict (+) a b
    _ -> return 10

