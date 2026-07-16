module Math.Parser where

import qualified Control.Monad.State as State
import qualified Data.Either as Either
import qualified Data.Bool   as Bool

import qualified Folklore.Lexer   as Lexer
import qualified Folklore.Grammar as Grammar
import qualified Folklore.Parser  as Parser

import qualified Math.Alphabet as Math.Alphabet
import qualified Math.AST      as Math.AST

type AST        = Math.AST.Expression
type Math       = Math.Alphabet.Type
type Pushdown a = Parser.Pushdown a
type Transition = State.StateT AST (Either.Either [Char]) (Pushdown Math)

parse :: [Lexer.Token Math] -> (Pushdown Math, AST) -> Either [Char] AST
parse []       (_       ,ast) = return ast
parse (tk:tks) (pushdown,ast) = State.runStateT allocate ast >>= parse tks
    where
    allocate :: Transition
    allocate = let currentGrammar = Parser.grammar pushdown
        in
        maybe (check tk pushdown) (update tk (pushdown,ast))
            $ State.runStateT (Grammar.transition (Lexer.label tk)) currentGrammar

update :: Lexer.Token Math -> (Pushdown Math,AST) -> ([Math],Grammar.Grammar Math) -> Transition
update token (pushdown,ast) (load,grammar) = do
    State.put  . maybe ast id 
        $ Math.AST.insert (Math.AST.toExpression token) ast
    return ((Parser.push load . Parser.pointer grammar) pushdown)

check :: Lexer.Token Math -> Pushdown Math -> Transition
check token pushdown = maybe (State.lift empty) consume (Parser.peak pushdown)
    where
    empty   = Left ("Invalid gramatical input (in): " ++ (show token))
    consume = Bool.bool (State.lift empty) (return (Parser.pop pushdown)) 
        . (==) (Lexer.label token)
    
