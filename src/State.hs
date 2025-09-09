module State where

import Graph.Automaton
import Graph.Grammar
import Parser
import Lexer

import Control.Monad

newtype State s a = State { runState :: s -> (a,s) }

state :: (s -> (a,s)) -> State s a
state = State

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure  = return
    (<*>) = ap

instance Monad (State s) where
    return :: a -> State s a
    return x = state (\s -> (x,s))

-- Garbage -- 
f :: Char -> Int -> (Double, Int)
f c x = (fromIntegral x / 2, x+1)

push :: Stack a -> a -> Stack a

-- recive an "state" 
state parser . (lexer . reverse . push Stack Char)
state . (lexer . reverse . flip (:) [])

type Token = [Char]
:: (Token, [Char]) -> (Info, (Token, [Char])) -- State (s -> (a,s))

([Char], Automaton) :: State Automaton [Char]

f :: Char -> ([Char], Automaton) -> State Automaton [Char]

