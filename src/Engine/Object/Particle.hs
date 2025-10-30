module Engine.Object.Particle where

import Engine.Math.Space

type Time = Double

data Coordinate = Coordinate
    { space :: Point 
    , time  :: Time
    } deriving Show

data Particle = Particle 
    { mass       :: Double
    , coordinate :: Coordinate
    , speed      :: Point
    } deriving Show

-- test

testParticle :: IO Particle
testParticle = do
    currentTime <- getTime
    return $ Particle 2 (Coordinate (-0.5,-0.1,0.3) $ currentTime / 1e8) (0.05,0.05,0.05)

changeSpace :: Point -> Coordinate -> Coordinate
changeSpace x c = Coordinate x (time c)

changeTime :: Time -> Coordinate -> Coordinate
changeTime x c = Coordinate (space c) x

type Speed = Point

changeSpeed :: Speed -> Particle -> Particle
changeSpeed spd p = Particle (mass p) (coordinate p) spd

displacement :: Time -> Speed -> Point
displacement t (x,y,z) = (x*t,y*t,z*t)

updateCoordinate :: Time -> Particle -> Particle
updateCoordinate t p = let
    timeUpdated  = changeTime t $ coordinate p
    spaceUpdated = changeSpace (sumPoint (space $ coordinate p) (displacement t (speed p))) 
        $ timeUpdated
    in Particle (mass p) spaceUpdated (speed p)


