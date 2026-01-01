module Struct.DBMS where
import Struct.System as System
import Prelude
import Data.Bool as Bool

type DBMS a = System.Graph a E
data E      = I | O 
    deriving (Eq)

create :: (Ord a) => a -> DBMS a
create = flip System.Vertex (\_ -> Nothing)

toList :: DBMS a -> [a]
toList g = (maybe [] toList (edge g O)) ++ [vert g] ++ (maybe [] toList (edge g I))

insert :: Ord a => a -> DBMS a -> DBMS a
insert x g = if x == (vert g) 
    then g { vert = x }
    else g { edge = insert' x (bool O I (x > vert g)) (edge g) }
-- aux
insert' :: Ord a => a -> E -> (E -> Maybe (DBMS a)) -> (E -> Maybe (DBMS a))
insert' x e f = \k -> (h k) k -- Changing a function
    where 
        -- h k = bool (f k) (Just . maybe (create x) (insert x) . f $ k) (k == e)
        h = bool f (Just . maybe (create x) (insert x) . f) . (==) e

-- todo : refactor!!
remove :: Ord a => a -> DBMS a -> Maybe (DBMS a)
remove x g
    | x < (vert g) = 
        let f O = edge g O >>= remove x
            f n = edge g n
        in Just $ System.Vertex (vert g) f
    | x > (vert g) = 
        let f I = edge g I >>= remove x
            f n = edge g n
        in Just $ System.Vertex (vert g) f
    | otherwise =
        case (edge g O, edge g I) of
            (Nothing, Nothing) -> Nothing
            (Just k,  Nothing) -> Just k
            (Just k,  Just y)  -> Just $ hold k (Just y)
            (Nothing, Just y)  -> Just y
-- aux
hold :: DBMS a -> Maybe (DBMS a) -> DBMS a
hold g Nothing  = g
hold g (Just k) = System.Vertex (vert k) f
    where
        f O = Just $ hold g (edge k O)
        f x = edge k x

find :: Ord a => a -> DBMS a -> Maybe (DBMS a)
find x g
    | x < (vert g) = edge g O >>= find x
    | x > (vert g) = edge g I >>= find x
    | otherwise    = Just g

