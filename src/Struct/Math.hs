module Struct.Math where
import Data.Time.Clock
import Data.Bool
import Data.List -- (uncons)

type Point = (Double, Double, Double)

fromIntegralPoint (x,y,z) = (fromIntegral x, fromIntegral y, fromIntegral z)
realToFracPoint   (x,y,z) = (realToFrac x, realToFrac y, realToFrac z)

getTime :: IO Double
getTime = do
    curretTime <- getCurrentTime
    return
        . (/1e11) . fromIntegral
        . diffTimeToPicoseconds $ utctDayTime curretTime

isInsideTriangle :: Point -> Point -> Point -> Point -> Bool
isInsideTriangle a b c p = let
    c1 = getCrossProduct (subtr b a) (subtr p a)
    c2 = getCrossProduct (subtr c b) (subtr p b)
    c3 = getCrossProduct (subtr a c) (subtr p c)
    result = [c1,c2,c3]
    in all (>= 0) result || all (<= 0) result
 
isInsidePolygon :: Point -> [Point] -> Bool
isInsidePolygon p lst = case uncons lst of
    Nothing     -> False
    Just (x,xs) -> g p x xs
-- remake:
g :: Point -> Point -> [Point] -> Bool
g p k []         = False
g p k l@(x:[])   = False
g p k l@(x:y:xs) = isInsideTriangle k x y p || g p k (y:xs)

-- End - Triangle Area

getX :: Point -> Double
getX (x,_,_) = x

getY :: Point -> Double
getY (_,y,_) = y

getZ :: Point -> Double
getZ (_,_,z) = z

type Maximum = Double
type Origin = Point

delta :: Double -> Double -> Double
delta i f = f - i

signal_ :: Double -> Double
signal_ k = k / getModule (k,0,0)

resize :: Maximum -> Point -> Point -- [?]
resize k (x,y,z) = let
    m = getModule (x,y,z) 
    x' =  (*) k $ x / m -- if x /= 0 then (cos $ getAngleX (x,y,z)) else 0
    y' =  (*) k $ y / m -- if y /= 0 then (cos $ getAngleY (x,y,z)) else 0
    z' =  (*) k $ z / m -- if z /= 0 then (cos $ getAngleZ (x,y,z)) else 0
    in (x',y',z') :: Point

getModule :: Point -> Double
getModule (x,y,z) = sqrt $ (x^2) + (y^2) + (z^2)

subtr :: Point -> Point -> Point
subtr (x0,y0,z0) (x1,y1,z1) = (x1-x0,y1-y0,z1-z0)

sumPoint :: Point -> Point -> Point
sumPoint (x0,y0,z0) (x1,y1,z1) = (x1+x0,y1+y0,z1+z0)

divPointBy :: Double -> Point -> Point
divPointBy k (x,y,z) = (x/k,y/k,z/k)

getDotProduct :: Point -> Point -> Double
getDotProduct (x,y,z) (i,j,k) = (x*i) + (y*j) + (z*k)

getCrossProduct :: Point -> Point -> Double
getCrossProduct (x0,y0,_) (x1,y1,_) = x0*y1 - x1*y0

getAngleBetween :: Point -> Point -> Double
getAngleBetween (x,y,z) (i,j,k) = 
    let mod1 = getModule (x,y,z)
        mod2 = getModule (i,j,k)
        prod = getDotProduct (x,y,z) (i,j,k)
     in if any ((==) 0) [mod1,mod2]
        then 0
        else acos (prod / (mod1 * mod2))

getAngleX :: Point -> Double
getAngleX (x,y,z)
    | x < 0  && y < 0 = (+) (pi) $ getAngleBetween (-1,0,0) (x,y,0)
    | x > 0  && y < 0 = (+) (pi) $ getAngleBetween (-1,0,0) (x,y,0)
    | x == 0 && y < 0 = (+) (pi) $ getAngleBetween (-1,0,0) (x,y,0)
    | otherwise      = getAngleBetween (1,0,0) (x,y,0)

getAngleY :: Point -> Double
getAngleY (x,y,z)
    | y < 0  && z < 0 = (+) (pi) $ getAngleBetween (0,-1,0) (0,y,z)
    | y > 0  && z < 0 = (+) (pi) $ getAngleBetween (0,-1,0) (0,y,z)
    | y == 0 && z < 0 = (+) (pi) $ getAngleBetween (0,-1,0) (0,y,z)
    | otherwise       = getAngleBetween (0,1,0) (0,y,z)

getAngleZ :: Point -> Double
getAngleZ (x,y,z)
    | z < 0  && x < 0 = (+) (pi) $ getAngleBetween (0,0,-1) (x,0,z)
    | z > 0  && x < 0 = (+) (pi) $ getAngleBetween (0,0,-1) (x,0,z)
    | z == 0 && x < 0 = (+) (pi) $ getAngleBetween (0,0,-1) (x,0,z)
    | otherwise       = getAngleBetween (0,0,1) (x,0,z)

type Rad = Double

rotateZ :: Point -> Rad -> Point
rotateZ (x,y,z) n = if x == 0 && y == 0 then (0,0,z) else
    let angle = getAngleX (x,y,z)
        range = getModule (x,y,0)
     in ((*) range $ cos (angle +n), (*) range $ sin (angle +n), z)
    
rotateY :: Point -> Rad -> Point
rotateY (x,y,z) n = if x == 0 && z == 0 then (0,y,0) else
    let angle = getAngleZ (x,y,z)
        range = getModule (x,0,z)
     in ((*) range $ sin (angle +n), y, (*) range $ cos (angle +n))
    
rotateX :: Point -> Rad -> Point
rotateX (x,y,z) n = if y == 0 && z == 0 then (x,0,0) else
    let angle = getAngleY (x,y,z)
        range = getModule (0,y,z)
     in (x, (*) range $ cos (angle +n), (*) range $ sin (angle +n))

