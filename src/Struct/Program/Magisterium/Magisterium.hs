module Struct.Program.Magisterium.Magisterium where

import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as GLUT
import qualified Struct.Math as Math
import qualified Struct.Screen as Screen
import qualified Struct.Space as Space
import qualified Struct.Compiler.Language.Grammar as Grammar
import qualified Struct.Compiler.Solver as Solver
import qualified Struct.Compiler.Parser as Parser
import qualified Struct.Compiler.Lexer as Lexer

data Player = Player
    { id         :: Integer
    , expression :: String
    , position   :: Position 
    } deriving Show

setExpression :: String -> Player -> Player
setExpression str player = player { expression = str }

data Position = Atack | Denfense
    deriving Show

-- data Projectile = Projectile 
--     { path :: Double -> Double }

data Magisterium = Magisterium
    { ground   :: Ground
    , progress :: Double
    , player1  :: Player
    , player2  :: Player 
    } deriving Show 

setPlayer1 :: Player -> Magisterium -> Magisterium
setPlayer1 player magis = magis { player1 = player }

-- round Manger --
-- getExpression :: String -> Player -> Player
-- getExpression str p = case Grammar.parse str of
--     Left  _ -> 
--     Right _ ->
-- 
-- animation :: 
------------------

magisterium :: Ground -> Magisterium
magisterium g = Magisterium g
    ((0))
    (Player 1 "3*x/2" Atack)
    (Player 1 "2*3-3*x*2/2*2" Atack)

data Ground = Ground
    { limit :: (Double,Double) 
    } deriving Show

-- data Graphics = Graphics
--     { :: String -> [Screen.Element] }

plot :: String -> Screen.Element
plot str = either (const $ newElem []) (flip drawExpression (-1,1)) (Grammar.parse str)
    where
    newElem = Screen.Element OpenGL.Points (OpenGL.Color3 0 1 (0 :: GLUT.GLfloat))

drawExpression :: Parser.Tree Lexer.Token -> (Double,Double) -> Screen.Element
drawExpression tree (x0,x) = mold 
    $ map (\k -> (k,Solver.solve' tree (k,0,0),0)) [x0,x0+(0.01)..x]
    where
    mold = Screen.Element (OpenGL.Points) (OpenGL.Color3 0.1 0.1 0.1) 

drawCartesian :: Ground -> [Screen.Element]
drawCartesian (Ground (x,y)) = xLines ++ yLines
    where
    ySize = 2/(2*y)
    xSize = 2/(2*x)
    xList = [-x,(-x)+xSize..x]
    yList = [-y,(-y)+ySize..y]
    xPair = foldl (\acc k -> [(k,-y,0),(k,y,0)]:acc) [] xList
    yPair = foldl (\acc k -> [(-x,k,0),(x,k,0)]:acc) [] yList
    xLines = map mold xPair
    yLines = map mold yPair
    mold = Screen.Element (OpenGL.Lines) (OpenGL.Color3 0.1 0.1 0.1) 

