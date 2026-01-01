module Struct.Space where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.Bool    as Bool (bool)
import Data.List    as List (foldl')
 
class Coordinate a where coord :: Axis -> a -> Coord

type Coord = Double
data Axis  = X | Y | Z

point :: Coordinate a => a -> (Coord, Coord, Coord)
point k = (,,) (coord X k) (coord Y k) (coord Z k)

znear :: (Coord, Coord, Coord) -> [(Coord, Coord, Coord)] -> (Coord, Coord, Coord)
znear = List.foldl' (\(x,y,z) (x',y',z') -> Bool.bool (x',y',z') (x,y,z) (z < z'))

zfar :: (Coord, Coord, Coord) -> [(Coord, Coord, Coord)] -> (Coord, Coord, Coord)
zfar = List.foldl' (\(x,y,z) (x',y',z') -> Bool.bool (x',y',z') (x,y,z) (z > z'))

