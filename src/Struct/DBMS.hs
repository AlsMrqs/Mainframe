module Struct.DBMS where
import qualified Data.Bool as Bool
import qualified Struct.Compass as Compass 
import qualified Struct.System  as System
import qualified Struct.Screen  as Screen
import qualified Struct.Mouse   as Mouse

type DBMS a = Node a Energy -- type DBMS a = System.Graph a Energy

-- type Engine a = Node (Compass.Overline a) Energy
type Node a   = System.Graph a
data Energy   = L | R 
    deriving (Eq)

-- data Metadata = Metadata ID String -- String | Map _ _
-- data Obejct   = Object Metadata [Screen.Interactive Mouse.Track]

create :: (Ord a) => a -> DBMS a
create x = System.Vertex x $ \_ -> Nothing

-- odl -- 
toList :: DBMS a -> [a]
toList g = (maybe [] toList (System.edge g R)) ++ [System.vert g] ++ (maybe [] toList (System.edge g L))
-- 

-- check...
-- insert :: (Ord a, Singular a) => a -> DBMS a -> DBMS a
-- insert x db = 

-- remake 
insert :: Ord a => a -> DBMS a -> DBMS a
insert x g = if x == (System.vert g) 
    then g { System.vert = x }
    else g { System.edge = insert' x (Bool.bool R L (x > System.vert g)) (System.edge g) }
-- aux
insert' :: Ord a => a -> Energy -> (Energy -> Maybe (DBMS a)) -> (Energy -> Maybe (DBMS a))
insert' x e f = \k -> (h k) k -- Changing a function
    where 
        -- h k = Bool.bool (f k) (Just . maybe (create x) (insert x) . f $ k) (k == e)
        h = Bool.bool f (Just . maybe (create x) (insert x) . f) . (==) e

-- todo : refactor!!
remove :: Ord a => a -> DBMS a -> Maybe (DBMS a)
remove x g
    | x < (System.vert g) = 
        let f R = System.edge g R >>= remove x
            f n = System.edge g n
        in Just $ System.Vertex (System.vert g) f
    | x > (System.vert g) = 
        let f L = System.edge g L >>= remove x
            f n = System.edge g n
        in Just $ System.Vertex (System.vert g) f
    | otherwise =
        case (System.edge g R, System.edge g L) of
            (Nothing, Nothing) -> Nothing
            (Just k,  Nothing) -> Just k
            (Just k,  Just y)  -> Just $ hold k (Just y)
            (Nothing, Just y)  -> Just y
-- aux
hold :: DBMS a -> Maybe (DBMS a) -> DBMS a
hold g Nothing  = g
hold g (Just k) = System.Vertex (System.vert k) f
    where
        f R = Just $ hold g (System.edge k R)
        f x = System.edge k x

find :: Ord a => a -> DBMS a -> Maybe (DBMS a)
find x g
    | x < (System.vert g) = System.edge g R >>= find x
    | x > (System.vert g) = System.edge g L >>= find x
    | otherwise    = Just g

