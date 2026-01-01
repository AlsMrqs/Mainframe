module Struct.Program where

import Graphics.Rendering.OpenGL hiding (Program)
import Graphics.UI.GLUT          hiding (Program)

import Control.Concurrent.MVar

import Data.List as List
import Data.Map  as Map

import Struct.Screen as Screen
import Struct.System as System 

data Program = Program 
    { name    :: String 
    , display :: DisplayCallback }

    -- , keyboardMouse :: KeyboardMouseCallback 
    -- , mouse         :: MouseButton -> KeyState -> Position -> IO () 
    -- , motion        :: Position -> IO () 
    -- , passiveMotion :: Position -> IO ()

instance Show Program where
    show = name

instance Ord Program where
    (>=) x y = (name x) >= (name y)
    (<=) x y = (name x) <= (name y)

instance Eq Program where
    (==) x y = (name x) == (name y)

