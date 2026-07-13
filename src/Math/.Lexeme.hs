module Math.Dictionary where

import qualified Graph as Graph
import qualified Folklore.Grammar.Lexer as Lexer
import qualified Math.Alphabet as Math

type Dictionary = Graph.Graph (Lexer.Token Math.Type) Char

graph :: Dictionary
graph = Graph.Vertex (Lexer.Token Math.None_ Lexer.Reject) (return . context)
    where
    context = \k -> case Math.token k of
        Math.Integer_     -> integer
        _                 -> root

integer :: Dictionary
integer 

