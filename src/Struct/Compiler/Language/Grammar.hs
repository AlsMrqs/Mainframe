module Struct.Compiler.Language.Grammar where

import Data.Maybe
-- import Data.Tree
import Data.List
import Prelude hiding (lex)

import Struct.Compiler.Language.Dictionary
import Struct.Compiler.Lexer
import Struct.Compiler.Parser

grammar = [ Production [ Terminal open
                       , Variable expression
                       , Terminal separator
                       , Variable expression
                       , Terminal separator
                       , Variable expression
                       , Terminal close ] ]

expression = 
    [ Production [Terminal preNumber, Variable expansion]
    , Production [Terminal preIdentifier, Variable expansion] ]

expansion = [ Production [Terminal preOperator, Variable expression]
            , Production [] ]

token :: ([Char],Automaton) -> Token
token (str,auto) = Token str (typeof auto)

lexer :: [Char] -> ([Char],Automaton) -> (Token,[Char])
lexer []     = flip (,) [] . token
lexer (x:xs) = either (flip (,) (x:xs) . fst) (lexer xs) . lexer' x

lex :: [Char] -> (Token,[Char])
lex = flip lexer ([],machine) . dropWhile ((==) ' ')

-- todo!!!!!!
parse' :: Token -> (Tree Token,[Symbol]) -> Either String (Tree Token,[Symbol])
parse' (Token x None_) _  = Left $ "Invalid token!" ++ (show x)
parse' token (tree,stack) = 
    let (accepted,newStack) = check token stack in
    if not accepted
        then Left $ (++) "Sintax error in: " (show token)
        else case alloc token tree of
            Nothing      -> Left  $ (++) "AST can't alloc: " (show token)
            Just newTree -> Right $ (,) newTree newStack

-- set the (Invalid token) Output!
parse'' :: [Char] -> (Tree Token,[Symbol]) -> Either String (Tree Token)
parse'' str (tree,stack) = fmap fst $
    foldl (\acc token -> either Left (parse' token) acc) 
        (Right (tree,stack)) 
        (pass str)

parse :: [Char] -> Either String (Tree Token)
parse str = parse'' str (Empty,[Variable expression])

pass :: [Char] -> [Token]
pass []  = []
pass str = if tokentype token == None_ then [token] else token : pass rest
    where
    (token,rest) = lex str

