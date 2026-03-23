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
import qualified Control.Concurrent as Concurrent (forkIO, killThread, MVar, modifyMVar_,threadDelay)
import qualified Struct.Graph as Graph
import qualified Control.Monad.State as State
import qualified Data.Bool as Bool
-- import Prelude hiding (player)

type Id         = Int
type Derivative = Function

newtype Function = Function { apply :: Double -> Double }

derive :: Function -> Function
derive func = Function f'
    where
    f  = apply func
    f' x = let h = 0.000001 in (f (x+h) - f x) / h

tangent :: Double -> Function -> Function
tangent x0 f = Function $ \x -> m*(x - x0) + y0
    where
    y0 = apply f x0
    m = apply (derive f) x0

perpendicular :: Double -> Function -> Function 
perpendicular x0 f = Function $ \x -> (negate x) / m
    where
    m = apply (derive f) x0

range :: [Double]
range = [-1,-1+1e-2..1]

isPerpendicular :: Double -> Function -> Function -> Bool
isPerpendicular x0 f g = fs == gs
    where
    fs = map (round . (*) 1e4 . apply (perpendicular x0 f)) range
    gs = map (round . (*) 1e4 . apply g) range

isDerivative :: Function -> Function -> Bool
isDerivative f g = f' == g'
    where
    f' = map (round . (*) 1e4 . apply (derive f)) range
    g' = map (round . (*) 1e4 . apply g) range

aim :: Function -> Double -> [Space.Point]
aim f x = map (Math.sumPoint (x, apply f x,0) .  flip Math.rotateZ alpha) 
    $ lxUp ++ lxDown ++ lyUp ++ lyDown
    where
    alpha = atan (apply (derive f) x)
    lxUp   = [(-0.05, 0.05,0),(-0.01, 0.05,0)] ++ [(0.05, 0.05,0),(0.01, 0.05,0)]
    lxDown = [(-0.05,-0.05,0),(-0.01,-0.05,0)] ++ [(0.05,-0.05,0),(0.01,-0.05,0)]
    lyUp   = [(-0.05, 0.05,0),(-0.05, 0.01,0)] ++ [(0.05, 0.05,0),(0.05, 0.01,0)]
    lyDown = [(-0.05,-0.05,0),(-0.05,-0.01,0)] ++ [(0.05,-0.05,0),(0.05,-0.01,0)]

-- newtype Analyst = Analyst { tactic :: Tactic }

newtype Offensive = Offensive { projectile :: (Maybe (Double,Function)) }
newtype Defensive = Defensive { analysis :: (Maybe Derivative) } 

instance Show Offensive where
    show (Offensive Nothing)  = "Offensive Nothing"
    show (Offensive (Just (t0,_)))  = "Offensive " ++ (show t0) ++ " (Just _)"

instance Show Defensive where
    show (Defensive Nothing) = "Defensive Nohting"
    show (Defensive (Just _)) = "Defensive (Just _)"

data Scale = Scale { x :: Double, y :: Double }
    deriving Show

-- data Trail = Trail Id [Space.Point] -- Database (Function.Point.Print)

type Trail = [Space.Point]
type Time = Double

trace :: Function -> Time -> Time -> Maybe Trail
trace f t0 t = Bool.bool (return newStack) Nothing (dt >= 2)
    where
    dt       = (t/100)-(t0/100)
    step     = negate 0.001
    newStack = foldl (flip (allocTrail f)) [] [1,1+step..1+(negate dt)]

allocTrail :: Function -> Time -> Trail -> Trail
allocTrail f t stack = (point:stack)
    where
    point = (t,(apply f) t,0)

data Gamester = Player1 | Player2 -- | InsertPositionP1 | InsertPositionP2
    deriving (Eq,Show)

test :: Game
test = Game 
    (Scale 5 5)
    (Player1)
    (Offensive Nothing)
    (Defensive Nothing)

---------------{ Game }-------------
 
data Game = Game 
    { scale   :: Scale 
    , player   :: Gamester 
    , player1 :: Offensive
    , player2 :: Defensive }

instance Show Game where
    show (Game s r p1 p2) = "Game \n"
        ++ (show s) ++ "\n"
        ++ (show r) ++ "\n"
        ++ (show p1) ++ "\n"
        ++ (show p2) ++ "\n"

-- insertPositionP1 :: (Double,Double) -> Game -> Game
-- insertPosition (x,y) status = case status of
--     InsertPositionP1 -> 
--     _                -> Nothing

insertOffensive :: Time -> Function -> Offensive -> Offensive
insertOffensive t0 f _ = Offensive (Just (t0,f))

insertDefensive :: Function -> Defensive -> Defensive
insertDefensive f _ = Defensive (Just f)

nextRound :: Game -> Game
nextRound game = case player game of
    Player1 -> game { player = Player2 }
    Player2 -> game { player = Player1 }

play :: Time -> Function -> Game -> Game
play t0 f game = case player game of
    Player1 -> nextRound $ game { player1 = insertOffensive t0 f (player1 game) }
    Player2 ->
        case (projectile . player1) game of
            Nothing      -> game
            Just (_,fP1) -> if isDerivative fP1 f
                then nextRound $ game { player2 = insertDefensive f (player2 game) } 
                else game { player2 = insertDefensive f (player2 game) }

type Input = String
type Error = String
type StateComputation = State.StateT Game (Either Error) String

-- todo
-- [+] Game (Msg) !!!
-- [+] Verify expression correctness
-- [+] Point distribution
-- [+] Round Counter
start :: Time -> Input -> StateComputation -- True (Main)
start t0 str = do
    case Grammar.parse str of
        Left  msg  -> State.lift (Left msg)
        Right tree -> do
            let f x = Solver.solve' tree (x,0,0)
            State.get >>= State.put . play t0 (Function f)
            return "Sucess!"

