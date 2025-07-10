module Arrow where

type Point = (Double,Double,Double)

data Arrow = Arrow
    { line :: [Point]   -- Lines
    , cone :: [Point] } -- Polygons -- todo
    
field :: Point -> Point

arrow :: (Point, Point) -> Arrow

