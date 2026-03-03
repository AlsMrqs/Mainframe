module Struct.Triangle where
import qualified Data.Bool as Bool
import qualified Data.List as List
import Struct.Space as Space

data Triangle = Triangle Point Point Point
    -- deriving Show

instance Show Triangle where
    show (Triangle a b c) = "Triangle" ++
        "\n" ++ (show a) ++
        "\n" ++ (show b) ++
        "\n" ++ (show c)

data Point = Point Space.Coord Space.Coord Space.Coord
    deriving Show

-- test
triangle0 :: Triangle
triangle0 = Triangle
    (Point 0.0 0.1 0.0)
    (Point 0.0 (-0.1) 0.0)
    (Point 0.0 0.0 0.1)
--

class Portal a where planet :: Planet -> a -> Point
data Planet = A | B | C

instance Portal Triangle where
    planet k (Triangle a b c) = case k of
        A -> a
        B -> b
        C -> c

instance Space.Coordinate Point where
    coord axis (Point x y z) = case axis of
        X -> x
        Y -> y
        Z -> z

toList :: Triangle -> [Point]
toList (Triangle a b c) = [a,b,c]

toTuple :: Point -> (Space.Coord, Space.Coord, Space.Coord)
toTuple (Point x y z) = (x,y,z)

toPolygon :: Triangle -> [(Space.Coord, Space.Coord, Space.Coord)]
toPolygon (Triangle a b c) = map toTuple [a,b,c]

toLines :: Triangle -> [(Space.Coord, Space.Coord, Space.Coord)]
toLines (Triangle a b c) = 
    [ toTuple a
    , toTuple b
    , toTuple b
    , toTuple c
    , toTuple c
    , toTuple a ]

generatePoint :: IO Point
generatePoint = do
    x <- Space.generateCoord
    y <- Space.generateCoord
    z <- Space.generateCoord
    return (Point x y z)

generateTriangle :: IO Triangle
generateTriangle = do
    a <- generatePoint
    b <- generatePoint
    c <- generatePoint
    return (Triangle a b c)

