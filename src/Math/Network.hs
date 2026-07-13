module Math.Network where

import qualified Graph    as Graph
import qualified Manager  as Manager
import qualified Data.Map as Map

import qualified Folklore.Lexer as Lexer
import qualified Folklore.Grammar as Grammar
import qualified Math.Alphabet as Math.Alphabet
import qualified Math.Neuron as Math.Neuron

import qualified Control.Monad.State as State

-- E  = n O | v O | a E' f
-- O  = p E | λ

expression :: Grammar.Grammar Math.Alphabet.Type
expression = Graph.Vertex [] f
    where
    f = \k -> case k of
        Math.Alphabet.Integer_  -> return operation
        Math.Alphabet.Double_   -> return operation
        Math.Alphabet.Variable_ -> return operation
        Math.Alphabet.Starter_  -> return $ 
            expression { Graph.vert = Math.Alphabet.Finisher_ : (Graph.vert expression) }
        _         -> Nothing

operation :: Grammar.Grammar Math.Alphabet.Type
operation = Graph.Vertex [] f
    where
    f = \k -> case k of
        Math.Alphabet.Operator_ -> return expression
        _         -> Nothing

