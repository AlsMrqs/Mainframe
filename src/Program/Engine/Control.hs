module Program.Engine.Control  where

import Graphics.UI.GLUT hiding (Cursor, cursor, Text)
import Control.Concurrent
import System.Random

import Data.Map as Map
import Data.List as List

import Struct.System as System
import Struct.Program as Program

keyboardMouse :: MVar (Program.System) -> KeyboardMouseCallback
keyboardMouse systemMVar key keyState mod pos = 
    case (key, keyState) of
        (Char '#', Down) -> putStrLn "Leaving Engine!"
        (Char '@', Down) -> putStrLn "Starting Bitmap!"
        _ -> return ()

mouse :: MVar Program.System -> MouseCallback
mouse systemMVar button keyState pos = putStrLn $ "Mouse Mouse: " ++ show pos

motion :: MVar Program.System -> MotionCallback
motion systemMVar pos = putStrLn $ "Mouse Motion: " ++ show pos

passiveMotion :: MVar Program.System -> MotionCallback
passiveMotion systemMVar pos = do
    system <- readMVar systemMVar
    let (Position x y) = pos
        x' = (+) (-1) $ (*) 2 (fromIntegral(x)/800)
        y' = (+) (1) $ (*) (-2) (fromIntegral(y)/600)
    putStrLn $ "Mouse Passive Motion: " ++ show (x',y')

-- pointToGLPoint__ :: Math.Point -> (GLfloat, GLfloat, GLfloat)
-- pointToGLPoint__ (x,y,z) = (realToFrac x, realToFrac y, realToFrac z)
-- 
-- draw__ :: (GLfloat, GLfloat, GLfloat) -> DisplayCallback 
-- draw__ (x,y,z) = vertex $ Vertex3 x y z
-- 
-- shiftPoint :: (Float, Float, Float) -> Math.Point
-- shiftPoint (a,b,c) = (realToFrac a, realToFrac b, realToFrac c)
--     
-- f :: Math.Point -> Math.Point -> [Math.Point] -> Bool
-- f p k []         = False
-- f p k l@(x:[])   = False
-- f p k l@(x:y:xs) = Math.isInsideTriangle k x y p || f p k (y:xs)
-- 
-- -- End | The real code --
-- 
-- isInRange :: ([GLfloat],[GLfloat]) -> (GLfloat,GLfloat) -> Bool
-- isInRange ([a,b],[c,d]) (x,y) = a <= x && x <= b && c <= y && y <= d
-- 
