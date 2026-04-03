module Struct.Program.Magisterium.Callback 
    ( module Magisterium
    , display 
    , renderPoints
    , newRGB )
where

import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as GLUT
import Struct.Program.Magisterium.Magisterium as Magisterium
import qualified Struct.Render as Render
import qualified Struct.Screen as Screen
import qualified Struct.Math as Math
import qualified Struct.Space as Space
import qualified Control.Concurrent as Concurrent

renderLines :: Screen.RGB GLUT.GLfloat -> [Space.Point] -> GLUT.DisplayCallback
renderLines rgb = Render.renderElement . Screen.linesToElement rgb

renderPoints :: Screen.RGB GLUT.GLfloat -> [Space.Point] -> GLUT.DisplayCallback
renderPoints rgb = Render.renderElement . Screen.pointsToElement rgb

newRGB :: Double -> Double -> Double -> Screen.RGB GLUT.GLfloat
newRGB r g b = OpenGL.Color3 (f r) (f g) (f b)
    where
    f = realToFrac

gameObjects :: Magisterium.Game -> [GLUT.DisplayCallback]
gameObjects game = 
    [ displayPositionP1 game
    , displayPositionP2 game
    , displayOffensive game
    , displayTrack game
    , displayBlock game 
    , displayNewGameTime game ]

display :: Magisterium.Game -> GLUT.DisplayCallback
display game = do
    displayAim game
    displayTrail game -- : todo it with invalid functions too!!!!!
    case status game of
        InsertPositionP1 -> return ()
        InsertPositionP2 -> displayPositionP1 game
        Launch -> (sequence_ . take 2 . gameObjects) game
        Track  -> (sequence_ . take 3 . gameObjects) game
        Block  -> (sequence_ . take 4 . gameObjects) game
        Waiting NewGame -> (sequence_ . gameObjects) game
        _ -> (sequence_ . gameObjects) game
    return ()

displayTrail :: Magisterium.Game -> GLUT.DisplayCallback
displayTrail game = do
    case (projectile . offensive . order) game of
        Nothing     -> return ()
        Just (_,f) -> maybe (pure ()) (renderPoints (newRGB 0.3 0.3 0.3))
            (Magisterium.trace f (0) (200)) -- : todo (fix time)!!!!
    return ()

displayAim :: Magisterium.Game -> GLUT.DisplayCallback
displayAim game = do
    t <- Math.getTime
    let lock (x,y) = map (Math.sumPoint (x,y,0)) Magisterium.aim
    let location = case (status game) of
            InsertPositionP1 -> lock (0,0)
            InsertPositionP2 -> lock $ (tuplePosition . position . player1) game
            Launch -> lock $ (tuplePosition . position . player2) game
            Track  -> lock $ (tuplePosition . position . player2) game
            Block -> do
                case (projectile . offensive . order) game of
                    Nothing     -> lock (0,0)
                    Just (t0,f) ->  -- solve time at displayTrack
                        let dt = (-) 1 $ (t/10000) - (t0/10000)
                        in follow f dt
            Waiting _ -> do
                case (projectile . offensive . order) game of
                    Nothing     -> lock (0,0)
                    Just (t0,f) ->  -- solve time at displayTrack
                        let t' = Magisterium.time game 
                            dt = (-) 1 $ (t'/10000) - (t0/10000)
                        in follow f dt
    renderLines (newRGB 1 1 0) location

displayPositionP1 :: Magisterium.Game -> GLUT.DisplayCallback
displayPositionP1 game = renderPoints (newRGB 1 1 0) [(x,y,0)]
    where 
    (x,y) = (tuplePosition . position . player1) game

displayPositionP2 :: Magisterium.Game -> GLUT.DisplayCallback
displayPositionP2 game = renderPoints (newRGB 1 0 1) [(x,y,0)]
    where
    (x,y) = (tuplePosition . position . player2) game

displayOffensive :: Magisterium.Game -> GLUT.DisplayCallback
displayOffensive game = do
    t <- Math.getTime
    case (projectile . offensive . order) game of
        Nothing     -> return ()
        Just (t0,f) -> do
            let t' = if Magisterium.isWaiting game then Magisterium.time game else t
            maybe (pure ()) (renderPoints (newRGB 0 1 0))
                (Magisterium.trace f (t0/100) (t'/100)) -- : todo (fix time)!!!!
    return ()

displayTrack :: Magisterium.Game -> GLUT.DisplayCallback
displayTrack game = do
    t <- Math.getTime
    case (projectile . offensive . order) game of
        Nothing     -> return ()
        Just (t0,f) -> 
            case (analysis . defensive . order) game of
                Nothing -> return ()
                Just f' -> do
                    let t' = if Magisterium.isWaiting game then Magisterium.time game else t
                        dt = (-) 1 $ (t'/10000) - (t0/10000)
                        line = Space.flatLine (dt, apply f dt, 0) 0.3 (atan (apply f' dt))
                    renderLines (newRGB 1 1 0) line

displayBlock :: Magisterium.Game -> GLUT.DisplayCallback
displayBlock game = do 
    t <- Math.getTime
    case (projectile . offensive . order) game of
        Nothing     -> return ()
        Just (t0,f) -> 
            case (projectile . offensive . order) game of
                Nothing -> return ()
                Just (t0,f) -> do
                    let dt = (-) 1 $ (t/10000) - (t0/10000)
                        line = Space.flatLine (dt, apply f dt, 0) 0.3 
                            (atan (apply (intersect f) dt))
                    renderLines (newRGB 1 0 0) line

displayNewGameTime :: Magisterium.Game -> GLUT.DisplayCallback
displayNewGameTime game = do
    t <- Math.getTime
    let t0 = Magisterium.time game
        dx = (0.5-(-0.5))/5
        line = map (\x -> (x,-0.5,0)) [-0.5,(0.5)-(dx * ((t/1e3)-(t0/1e3)))]
    renderLines (newRGB 0.2 0.2 1) line

