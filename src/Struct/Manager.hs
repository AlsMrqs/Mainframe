module Struct.Manager where
import Struct.System as System
import Data.List (delete)

type Manager a = Maybe (System.Graph a Energy)
data Energy    = Prev | Next

create :: a -> Manager a
create x = k where k = Just $ System.Vertex x (\_ -> k)

empty :: Manager a
empty = fromList []

toList :: Eq a => Manager a -> [a]
toList x = case x of
    Nothing -> []
    Just k  -> (vert k) : (f k $ next x)
-- aux
f :: Eq a => Graph a Energy -> Manager a -> [a]
f x y = case y of
    Nothing -> []
    Just k  -> if (vert x) == (vert k) then [] else (vert k) : (f x $ next y)

fromList :: [a] -> Manager a
fromList []     = Nothing
fromList (x:xs) = Just fst
    where
        (lst,nxt) = link xs fst fst
        fst = System.Vertex x $ \d -> case d of
            Prev -> Just lst
            Next -> Just nxt
-- aux
link :: [a] -> Graph a Energy -> Graph a Energy -> (Graph a Energy, Graph a Energy)
link     [] fst lst = (lst,fst)
link (x:xs) fst lst = (prv,new)
    where
        (prv,nxt) = link xs fst new
        new = System.Vertex x $ \d -> case d of
            Prev -> Just lst
            Next -> Just nxt

insert :: Eq a => a -> Manager a -> Manager a
insert x = fromList . (:) x . delete x . toList

remove :: Eq a => a -> Manager a -> Manager a
remove x = fromList . delete x . toList

next :: Manager a -> Manager a
next = maybe Nothing (cross Next) 

prev :: Manager a -> Manager a
prev = maybe Nothing (cross Prev)

