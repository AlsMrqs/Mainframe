module Math.Dictionary where

import qualified Graph as Graph
import qualified Folklore.Grammar.Lexer as Lexer
import qualified Math.Alphabet as Math

type Neuron = Graph.Graph (Lexer.Kind Math.Type) Char

start :: Neuron
start = Graph.Vertex (Lexer.Kind Math.None_ Lexer.Reject) context
    where
    context = \k -> case Math.token k of
        Math.Integer_     -> return integer
        _                 -> Nothing

integer :: Neuron
integer = Graph.Vertex (Lexer.Kind Math.Integer_ Lexer.Accept) context
    where
    context = \k -> case Math.token k of
        Math.Integer_     -> return integer
        Math.Punctuation_ -> return point
        _                 -> Nothing

point :: Neuron
point = Graph.Vertex (Lexer.Kind Math.Punctuation_ Lexer.Reject) context
    where
    context = \k -> case Math.token k of
        Math.Integer_ -> return double
        _ -> Nothing

double :: Neuron
double = Graph.Vertex (Lexer.Kind Math.Double_ Lexer.Accept) context
    where
    context = \k -> case Math.token k of
        Math.Integer_ -> return double
        _ -> Nothing

