module Struct.Cube where
import Struct.Triangle as Triangle
import Struct.Space    as Space

data Cube = Cube 
    { front :: Space.Coord
    , back  :: Space.Coord
    , up    :: Space.Coord
    , down  :: Space.Coord
    , left  :: Space.Coord
    , right :: Space.Coord 
    } -- deriving (Show)

instance Show Cube where
    show (Cube f b u d l r) = "Cube" ++
        "\nfont:"  ++ (show f) ++
        "\nback:"  ++ (show b) ++
        "\nup:"    ++ (show u) ++
        "\ndown:"  ++ (show d) ++
        "\nleft:"  ++ (show l) ++
        "\nright:" ++ (show r)

seize :: Triangle.Triangle -> Cube
seize t = Cube
    { front = Space.coord Z $ select Space.Z (>) (Triangle.toList t)
    , back  = Space.coord Z $ select Space.Z (<) (Triangle.toList t)
    , up    = Space.coord Y $ select Space.Y (>) (Triangle.toList t)
    , down  = Space.coord Y $ select Space.Y (<) (Triangle.toList t)
    , left  = Space.coord X $ select Space.X (<) (Triangle.toList t)
    , right = Space.coord X $ select Space.X (>) (Triangle.toList t)
    }

toLines :: Cube -> [(Space.Coord, Space.Coord, Space.Coord)]
toLines (Cube f b u d l r) = 
    [ (l,u,f)
    , (r,u,f)
    , (r,u,f)
    , (r,d,f)
    , (r,d,f)
    , (l,d,f)
    , (l,d,f)
    , (l,u,f) ] ++
    [ (l,u,b)
    , (r,u,b)
    , (r,u,b)
    , (r,d,b)
    , (r,d,b)
    , (l,d,b)
    , (l,d,b)
    , (l,u,b) ] ++
    [ (l,u,f)
    , (l,u,b)
    , (r,u,f)
    , (r,u,b)
    , (l,d,f)
    , (l,d,b)
    , (r,d,f)
    , (r,d,b) ]

data Test = Test 
    { triangle :: Triangle.Triangle 
    , cube     :: Cube } deriving (Show)

