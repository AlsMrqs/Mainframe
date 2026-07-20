module Math.Parser where

import qualified Control.Monad.State as State
import qualified Data.Either as Either
import qualified Data.Bool   as Bool

import qualified Folklore.Lexer   as Lexer
import qualified Folklore.Grammar as Grammar
import qualified Folklore.Parser  as Parser

import qualified Math.Alphabet as Math.Alphabet
import qualified Math.Neuron   as Math.Neuron
import qualified Math.Network  as Math.Network
import qualified Math.AST      as Math.AST

type AST        = Math.AST.Expression
type Math       = Math.Alphabet.Type
type Pushdown a = Parser.Pushdown a
type Transition = State.StateT AST (Either.Either [Char]) (Pushdown Math)

parse :: [Char] -> Either.Either [Char] Math.AST.Expression
parse str = Math.Neuron.lexer str >>= \tks -> parser tks pushdAutomat
    where
    pushdAutomat :: (Pushdown Math,AST)
    pushdAutomat = (Parser.Pushdown [] Math.Network.expression, Math.AST.Free)

parser :: [Lexer.Token Math] -> (Pushdown Math,AST) -> Either [Char] AST
parser []       (_       ,ast) = return ast
parser (tk:tks) (pushdown,ast) = State.runStateT allocate ast >>= parser tks
    where
    allocate :: State.StateT AST (Either.Either [Char]) (Pushdown Math)
    allocate = maybe (check tk pushdown) (update tk (pushdown,ast)) result

    result :: Maybe ([Math.Alphabet.Type],Grammar.Grammar Math)
    result = State.runStateT (Grammar.transition (Lexer.label tk)) currentGrammar

    currentGrammar :: Grammar.Grammar Math.Alphabet.Type
    currentGrammar = Parser.grammar pushdown

update :: Lexer.Token Math -> (Pushdown Math,AST) -> 
    ([Math],Grammar.Grammar Math) -> Transition
update token (pushdown,ast) (load,grammar) = do
    State.put  . maybe ast id $ Math.AST.insert (Math.AST.toExpression token) ast
    return ((Parser.push load . Parser.pointer grammar) pushdown)

-- Close the (Enclosed _ _ _ _) !!!
-- ...
-- Need recognize type: ( | [ | {

check :: Lexer.Token Math -> Pushdown Math -> Transition
check token pushdown = do
    maybe (State.lift empty) consume (Parser.peak pushdown)
    where
    empty   = Left ("Invalid Grammatic input (in): " ++ (show token))
    consume = Bool.bool (State.lift empty) realize -- (return (Parser.pop pushdown)) 
        . (==) (Lexer.label token)
    realize = do
        ast <- State.get
        -- {turnOffEnclosed} : -> it could be another function !!!!!!!
        case Math.AST.turnOffEnclosed (Lexer.label token) ast of
            Nothing     -> State.lift (Left "Can't close Enclosed!") -- State.put ast
            Just newAst -> State.put newAst
                >> return (Parser.pop pushdown)

