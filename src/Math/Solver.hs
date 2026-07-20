module Math.Solver where

import qualified Math.AST as Math.AST
import qualified Data.Map as Map

type Expression = Math.AST.Expression
type Dictionary = Map.Map [Char] Double
type Function = Double -> Double -> Double

binOp :: Dictionary -> Function -> Expression -> Expression -> Either [Char] Double
binOp dict op a b = solve dict a
    >>= \l -> solve dict b
    >>= \r -> return (op l r)

solve :: Dictionary -> Expression -> Either [Char] Double
solve dict expr = case expr of
    (Math.AST.Multiplication a b) -> binOp dict (*) a b
    (Math.AST.Division a b)       -> binOp dict (/) a b
    (Math.AST.Subtraction a b)    -> binOp dict (-) a b
    (Math.AST.Addition a b)       -> binOp dict (+) a b
    (Math.AST.Enclosed _ x _ _)   -> solve dict x
    (Math.AST.Variable str) -> maybe (Left varError) return (Map.lookup str dict)
    (Math.AST.Value x)      -> return x
    _ -> return 10
    where

    varError :: String
    varError = "Variable not found!"

