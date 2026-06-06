module Math.Network where

import qualified Graph as Graph
import qualified Manager as Manager
import qualified Folklore.Lexer as Lexer
import qualified Folklore.Grammar as Grammar
import qualified Math.Alphabet as Math.Alphabet
import qualified Math.Neuron as Math.Neuron
import qualified Data.Map as Map

node :: Math.Alphabet.Type -> Bool
node Math.Alphabet.Operator_ = True
node _ = False

-- arity :: Lexer.Token Math.Alphabet.Type -> Int
-- arity (Lexer.Token Math.Alphabet.Operator_ str) = 
-- arity (Lexer.Token _ _) = 0

type Expression = Grammar.Spark Math.Alphabet.Type

type Names = Map.Map (Lexer.Token Math.Alphabet.Type) Expression

_ :: Lexer.Token Math.Alphabet.Type -> Expression -> Expression
_ 

Manager.Manager Expression

