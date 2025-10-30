module Program.Bitmap where

import Engine.Core
import Graphics.UI.GLUT hiding (Program)
import Graphics.Rendering.OpenGL hiding (Program)
import Control.Concurrent.MVar

{-
 - grid (zoom)
 - mouse (interface/api)
 - database
 - commands
 -}

bitmap = Program
    { name = "bitmap"
    , keyboardMouse = bitmapKeyboardMouse
    , mouse = bitmapMouse 
    , motion = bitmapMotion 
    , frame = gridLines } -- <- for while it is read only

-- bitmapMotion
bitmapMotion :: MVar Node -> Position -> IO ()
bitmapMotion nodeMVar pos = do
    putStrLn $ "Mouse | "++(show pos)

-- bitmapMouse 
bitmapMouse :: MVar Node -> MouseButton -> KeyState -> Position -> IO ()
bitmapMouse nodeMVar button state pos = do
    putStrLn $ "Mouse Button: " ++ show button
    putStrLn $ "Mouse State: " ++ show state
    putStrLn $ "Mouse Position: " ++ show pos
    putStrLn $ "Mouse Quadrant: " ++ show (quadrant pos)

-- bitmapKeyboardMouse
bitmapKeyboardMouse :: MVar Node -> KeyboardMouseCallback
bitmapKeyboardMouse mvarNode key keyState mod pos =
    case (key, keyState) of
        (Char '#', Down) -> do
            putStrLn $ "Keyboard | "++(show (key,keyState))
            leaveMainLoop
        _ -> return ()

gridLines :: [(GLfloat,GLfloat,GLfloat)]
gridLines = let
    lns = map ((+) $ 0.2*(-4)) . take 9 $ [0.0,0.2..]
    -- in [(x,-1.0,0.0) | x <- lns]
    in foldl (\acc x -> (x,-1.0+0.2,0):(x,1.0-0.2,0):(-1.0+0.2,x,0):(1.0-0.2,x,0):acc) [] lns

-- Just a example solution:

type BaseScreen = [[Bool]]

baseScreen = replicate 8 . take 8 $ cycle [False]

paintIn :: Int -> Int -> BaseScreen -> BaseScreen
paintIn x 0 (l:ls) = f x l : ls
    where
    f 0 (b:bs) = True : bs
    f x (b:bs) = b : f (x-1) bs
paintIn x y (l:ls) = l : paintIn x (y-1) ls

--  = [(0+15),(15+15)..(300-15)]

quadrant :: Position -> (Float,Float)
quadrant (Position x y) = ((fromIntegral(x)/30.0) - 5.0, (fromIntegral(y)/30.0) - 5.0)
-- Where is the Mouse!!! ???

{-Database - Bitmap(Core.Screen)(8x8)#16b-}
    
-- type PixelBlock = [(GLFloat,GLFloat,GLFloat)]
-- 
-- pixelBlock :: PixelBlock
-- pixelBlock 

-- todo: 1° Doing -> render (gridLines)
-- todo: mouse (dragging position)

