module Main where

import qualified Folklore.Lexer   as Lexer
import qualified Folklore.Grammar as Grammar
import qualified Folklore.Parser  as Parser
import qualified Math.Alphabet as Math.Alphabet
import qualified Math.Neuron   as Math.Neuron
import qualified Math.Network  as Math.Network
import qualified Math.AST      as Math.AST
import qualified Math.Parser   as Math.Parser
import qualified Math.Solver   as Math.Solver

import qualified Control.Monad.State as State
import qualified Data.Map    as Map
import qualified Data.Either as Either

main :: IO ()
main = do
    input <- (putStrLn "Math: " >> getLine)
    return ()

lex :: [Char] -> (Lexer.Token (Lexer.Kind Math.Alphabet.Type),[Char])
lex = Lexer.lex Math.Neuron.start . dropWhile ((==) ' ')

lexer :: [Char] -> Either [Char] [Lexer.Token Math.Alphabet.Type]
lexer []  = return []
lexer str = let (tokenKind, remainder) = Main.lex str
    in 
    if Lexer.flag (Lexer.label tokenKind) == Lexer.Reject
        then Left ("Invalid input! " ++ show (tokenKind))
        else lexer remainder >>= return . (:) (fmap Lexer.unkind tokenKind)

parse :: [Char] -> Either.Either [Char] Math.AST.Expression
parse str = Main.lexer str 
    >>= \tks -> Math.Parser.parse tks $
        (,) (Parser.Pushdown [] Math.Network.expression) Math.AST.Free


