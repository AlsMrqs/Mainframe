module Struct.Tree where
import qualified Struct.System as System
subtree = System.edge
node    = System.vert

type Tree a = System.Graph (Node a) Energy
data Node a = Node { weight :: Int, payload :: a } deriving Show
data Energy = L | R
    deriving (Show)

change :: a -> Node a -> Node a
change x n = n { payload = x }

singleton :: a -> Tree a
singleton x = System.Vertex (Node 1 x) (\_ -> Nothing)

toList :: Tree a -> [a]
toList tree = 
    (maybe [] toList $ subtree tree L) ++
    [payload $ node tree] ++
    (maybe [] toList $ subtree tree R)

-- insert :: (Ord a, Eq a) => a -> Tree a -> Tree a
-- insert x tree 
--     | x == node tree = tree { System.vert = x }
--     | otherwise      = tree { System.edge = newSubtree }
--         where 
--         newSubtree
--             | x < node tree = 
--                 let f L = insertInSubtree x (subtree tree L)
--                     f R = subtree tree R
--                     in f
--             | x > node tree = 
--                 let f L = subtree tree L
--                     f R = insertInSubtree x (subtree tree R)
--                     in f

-- insertInSubtree :: (Ord a, Eq a) => a -> Maybe (Tree a) -> Maybe (Tree a)
-- insertInSubtree x subTree = case subTree of
--     Nothing -> return $ singleton x
--     Just st -> return $ insert x st
    
-- remove :: (Ord a, Eq a) => a -> Tree a -> Maybe (Tree a)
-- find :: (Ord a, Eq a) => a -> Tree a -> Maybe a


