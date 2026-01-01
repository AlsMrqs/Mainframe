module Engine.Math.Dynamics where

import Engine.Math.Space
import Engine.Object.Particle

type Force = (Point -> Point)

react :: Force -> Particle -> IO Particle
react f p = do
    let 
        spaceCoord   = space . coordinate
        force k      = subtr (spaceCoord k) (f $ spaceCoord k)
        applyField k = changeSpeed (sumPoint (speed k) . divPointBy (mass k) $ force k) k
    newTime <- getTime
    return $ updateCoordinate (newTime / 1e8) . applyField $ p

