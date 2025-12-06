module Program.Bitmap.Script.Motion where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Concurrent.MVar

import Engine.Core

bitmapMotion :: MVar Node -> MotionCallback
bitmapMotion nodeMVar pos = putStrLn $ "Mouse | "++(show pos)

bitmapPassiveMotion :: MVar Node -> MotionCallback
bitmapPassiveMotion nodeMVar pos = putStrLn $ "PassiveMotion | "++(show pos)
