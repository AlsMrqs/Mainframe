module Block where

import Graph.Grammar

type Expression = [[Char]]

newtype Block a = Block { content :: a }
    deriving Show

instance Functor Block where
    fmap f (Block x) = Block (f x)

stack :: Expression -> Block [Expression] -> Block [Expression]
stack x = fmap ((:) x)

push :: Token -> Expression -> Expression
push = (:)

mount :: Token -> Block [Expression] -> Block [Expression]
mount token block
    | null $ content block = Block [[token]]
    | otherwise = let (x:xs) = content block in Block ((push token x):xs)

alloc :: Block [Expression] -> Block [Expression]
alloc = stack []
