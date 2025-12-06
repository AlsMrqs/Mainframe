module Program.Bitmap.Design where

import Graphics.Rendering.OpenGL
-- import Graphics.Rendering.GLUT

gridLines :: [(GLfloat,GLfloat,GLfloat)]
gridLines = let
    lns = map ((+) $ 0.2*(-4)) . take 9 $ [0.0,0.2..]
    in foldl (\acc x -> (x,-1.0+0.2,0):(x,1.0-0.2,0):(-1.0+0.2,x,0):(1.0-0.2,x,0):acc) [] lns

-- Just a example solution: 
-- !!!!! rewrite it to a simple solution!!!!! VV (PLEASE!!!)

type BaseScreen = [[Bool]]

baseScreen = replicate 8 . take 8 $ cycle [False]

-- paintIn :: Int -> Int -> BaseScreen -> BaseScreen
-- paintIn x y bs@(l:ls)
--     | any (<0) [x,y] = bs
--     | y >= length bs || x >= length l = bs
-- paintIn x 0 (l:ls) = f x l : ls
--     where
--     f 0 (b:bs) = True : bs
--     f x (b:bs) = b : f (x-1) bs
-- paintIn x y (l:ls) = l : paintIn x (y-1) ls

paintOut :: Position -> [(GLfloat,GLfloat,GLfloat)] -- Polygon Set
paintOut pos = let
    (x,y) = (\(Position x y) -> (x,y)) pos
    x' = (*) 0.2 $ fromIntegral $ (truncate (fromIntegral(x) / 30.0)) - 5
    y' = (*) 0.2 $ fromIntegral $ ((*) (-1) $ truncate (fromIntegral(y) / 30.0)) + 5
    in [ ((x'), (y'),0.0)
        , ((x')+0.2, (y'), 0.0)
        , ((x')+0.2, (y')-0.2, 0.0)
        , ((x'), (y')-0.2, 0.0) ] 

quadrant :: Position -> (Float,Float)
quadrant (Position x y) = ((fromIntegral(x)/30.0), (fromIntegral(y)/30.0))

