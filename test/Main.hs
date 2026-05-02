module Main where

import qualified Matrix as Matrix
import qualified Solve as Solve
import qualified Struct.Compiler.Language.Grammar as Grammar
import qualified Struct.Compiler.Solver as Solver

main :: IO ()
main = do
    (xp1,yp1) <- Solve.randomPair
    (xp2,yp2) <- Solve.randomPair
    degree <- Solve.randomDegree
    expression <- Solve.solution degree (xp1,yp1) (xp2,yp2)
    print (xp1,yp1)
    print (xp2,yp2)
    putStrLn expression
    print $ Grammar.parse expression
    print $ fmap Solve.roundIt $ Solver.solve expression (xp1,yp1,0)
    print $ fmap Solve.roundIt $ Solver.solve expression (xp2,yp2,0)
    
