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
import qualified Text.Read as Read
import qualified System.Random as Random

-----------------{ Function } ------------------
newtype Function = Function { apply :: Double -> Double }

type Trail = [Space.Point]
type Time = Double

toSecond :: Time -> Time
toSecond = flip (/) 1e3 -- wtf

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
perpendicular x0 f = Function $ \x -> (negate(1/m))*(x - x0) + y0
    where
    y0 = apply f x0
    m  = apply (derive f) x0

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

--------------------------{ Config }----------------------------
data Scale = Scale Double Double
    deriving Show
--------------------------{ GamePlay }--------------------------
type Derivative = Function

data Offensive = Offensive { projectile :: (Maybe (Double,Function)) }
data Defensive = Defensive { analysis :: (Maybe Derivative) } 

newOffensive :: Time -> Function -> Offensive
newOffensive t0 = Offensive . pure . (,) t0

newDefensive :: Derivative -> Defensive
newDefensive = Defensive . pure

instance Show Offensive where
    show (Offensive (Just (t0,_))) = "Offensive " ++ (show t0) ++ " (Just _)"
    show (Offensive Nothing)       = "Offensive Nothing"

instance Show Defensive where
    show (Defensive (Just _)) = "Defensive (Just _)"
    show (Defensive Nothing)  = "Defensive Nohting"

data Freeze = Pause | NewGame
    deriving (Eq,Show)

data Status = 
    InsertPositionP1 
    | InsertPositionP2 
    | Launch
    | Track
    | Block
    | Waiting Freeze
    deriving (Eq,Show)

-------------------{ Game Instance }---------------
test :: Time -> Game
test t0 = Game 
    (Scale 5 5)
    (t0)
    (InsertPositionP1)
    (A (Offensive Nothing) (Defensive Nothing))
    (Player [] 0 (Position 0 0))
    (Player [] 0 (Position 0 0))

---------------{ Game }-------------
data Position = Position { x :: Double, y :: Double } deriving Show

tuplePosition :: Position -> (Double,Double)
tuplePosition pos = (,) (x pos) (y pos)

data Order = A Offensive Defensive | B Defensive Offensive
    deriving Show

offensive :: Order -> Offensive
offensive order = case order of
    A off _ -> off
    B _ off -> off

defensive :: Order -> Defensive
defensive order = case order of
    A _ def -> def
    B def _ -> def

insertOffensive :: Time -> Function -> Order -> Order
insertOffensive  t0 f order = let newProjectile = newOffensive t0 f in
    case order of
        A off def -> A newProjectile def
        B def off -> B def newProjectile 

insertDefensive :: Function -> Order -> Order
insertDefensive f order = let newAnalysis = newDefensive f in
    case order of
        A off def -> A off newAnalysis
        B def off -> B newAnalysis off

data Game = Game 
    { scale   :: Scale 
    , time    :: Time
    , status  :: Status
    , order   :: Order
    , player1 :: Player
    , player2 :: Player }

changeTime :: Time -> Game -> Game
changeTime t0 game = game { time = t0 }

instance Show Game where
    show (Game s t r or p1 p2) = "Game \n"
        ++ (show s) ++ "\n"
        ++ (show $ toSecond t) ++ "\n"
        ++ (show r) ++ "\n"
        ++ (show or) ++ "\n"
        ++ (show p1) ++ "\n"
        ++ (show p2) ++ "\n"

data Player = Player
    { nickName :: String
    , points   :: Int
    , position :: Position } deriving Show

insertName :: String -> Player -> Player
insertName str player = player { nickName = str }

insertPosition :: Position -> Player -> Player
insertPosition newPos player = player { position = newPos }

offensivePlayer :: Game -> Player
offensivePlayer game = case order game of
    A off _ -> player1 game
    _       -> player2 game

defensivePlayer :: Game -> Player
defensivePlayer game = case order game of
    A _ def -> player2 game
    _       -> player1 game

--------------------{ GameRunTime }-----------------------

-- start :: Time -> Input -> StateComputation -- True (Main)
-- start t0 str = do
--     case Grammar.parse str of
--         Left  msg  -> State.lift (Left msg)
--         Right tree -> do
--             let f x = Solver.solve' tree (x,0,0)
--             State.get >>= State.put . play t0 (Function f)
--             return "Sucess!"

nextRound :: Time -> Game -> Game
nextRound t0 game = case status game of
    InsertPositionP1 -> changeTime t0 $ game { status = InsertPositionP2 }
    InsertPositionP2 -> changeTime t0 $ game { status = Launch }
    Launch           -> changeTime t0 $ game { status = Track }
    Track            -> game { status = Block }
    Block            -> changeTime t0 $ game { status = InsertPositionP1 }

selectRound :: Status -> Game -> Game
selectRound newStatus game = game { status = newStatus }

type Input = String
type Error = String
type Message = String
type StateComputation = State.StateT Game (Either Error) Message

-- play :: Time -> String -> Game -> Either Error (String,Game)
play :: Time -> String -> Game -> Either Error (String,Game)
play t str game = case status game of
    InsertPositionP1 -> readPoint str    
        >>= pure . fmap (nextRound t) . flip insertPositionP1 game
    InsertPositionP2 -> readPoint str    
        >>= fmap (fmap (nextRound t)) . flip insertPositionP2 game
    Launch           -> readFunction str 
        >>= fmap (fmap (nextRound t)) . flip (launch t) game
    Track            -> readFunction str  -- todo Block
        -- >>= fmap (fmap (nextRound t)) . flip track game
        >>= fmap (fmap (newGame t)) . flip track game
    -- loop
    _                -> pure (str,game)
    -- Block            ->

---------------------{ Player Positioning }---------------------todo!!!!!!
launch :: Time -> Function -> Game -> Either Error (String,Game)
launch t0 f game = 
    let (xOff,yOff) = (tuplePosition . position . offensivePlayer) game
        (xDef,yDef) = (tuplePosition . position . defensivePlayer) game
        origin      = apply f xOff == yOff
        hit         = apply f xDef == yDef
        newOrder    = (insertOffensive t0 f . order) game
    in if not hit || not origin
        then Left "Invalid projectile!"
        else pure . (,) "Expression accepted!" $ game { order = newOrder }

track :: Function -> Game -> Either Error (String,Game)
track f game = 
    let newOrder = (insertDefensive f . order) game
        launcher = (projectile . offensive . order) game
    in case launcher of
        Nothing          -> pure . (,) "Nothing to track! Restarting!" 
            $ selectRound Launch game
        Just (_,project) ->
            if (not . isDerivative project) f
                then Left "Untracked projectile!"
                else pure . (,) "Projectile Tracked!" $ game { order = newOrder }

---------------------------------------------------------------

readFunction :: Input -> Either Error Function 
readFunction str = fmap (Function . f) (Grammar.parse str)
    where
    f tree x = Solver.solve' tree (x,0,0)

readPoint :: String -> Either Error Position
readPoint str = maybe (Left "Invalid input!") tryPosition
    (Read.readMaybe str :: Maybe (Double,Double))

tryPosition :: (Double,Double) -> Either Error Position
tryPosition (a,b) = Bool.bool (Left "Out of range!") (Right $ Position a b) (validPosition (a,b))

validPosition :: (Double,Double) -> Bool
validPosition (a,b) = (-1 < a && a < 1 && a /= 0) && (-1 < b && b < 1)
        
insertPositionP1 :: Position -> Game -> (String,Game)
insertPositionP1 pos game = (,) "Player1 choose a Position!" $ game { player1 = newPlayer1 }
    where
    newPlayer1 = insertPosition pos (player1 game)

insertPositionP2 :: Position -> Game -> Either Error (String,Game)
insertPositionP2 pos game = if sameSideYAxis xP1 xP2
    then Left  $ "Players can't stay in the same side of Y axis!"
    else Right . (,) "Position accepted!" $ game { player2 = newPlayer2 }
    where
    xP1 = (x . position . player1) game
    xP2 = (x pos)
    newPlayer2 = (insertPosition pos . player2) game

sameSideYAxis :: Double -> Double -> Bool
sameSideYAxis x1 x2 = (==) (x1 / abs x1) (x2 / abs x2)

--------------------{ Time System }--------------------(seconds)
timeOver :: Time -> Game -> Maybe (Message,Game)
timeOver t game = do
    case status game of
        InsertPositionP1 -> event t 30 (randomPosMsg,insertRandomPositionP1) game
        InsertPositionP2 -> event t 30 (randomPosMsg,insertRandomPositionP2) game
        Launch           -> event t 60 (launchMsg,newGame) game
        Track            -> event t 60 (trackMsg,newGame) game
        Block            -> event t 60 (blockMsg,newGame) game
        Waiting Pause    -> Nothing
        Waiting NewGame  -> event t 5 (newGameMsg,newGame) game

randomPosMsg = "Random position inserted!"
launchMsg = "Launch time is over!"
trackMsg = "Track time is over!"
blockMsg = "Block time is over!"
newGameMsg = "Shift player for a new round -> <- ?"

type GameAction = Game -> (Message,Game)

event :: Time -> Time -> (Message, Time -> Game -> Game) -> Game -> Maybe (Message,Game)
event t limit (msg,f) game = Bool.bool Nothing (Just (msg,newGame)) activated
    where 
    activated = toSecond (t - t0) >= limit
    t0 = time game
    newGame = f t game

insertRandomPositionP1 :: Time -> Game -> Game
insertRandomPositionP1 t game = nextRound t $ game { player1 = newPlayer }
    where
    newPlayer = insertPosition (Position 0.1 0) (player1 game)

insertRandomPositionP2 :: Time -> Game -> Game
insertRandomPositionP2 t game = nextRound t $ game { player2 = newPlayer }
    where
    xP1 = (x . position . player1) game
    xP2 = Bool.bool 0.5 (-0.5) (sameSideYAxis xP1 0.5)
    newPlayer = insertPosition
        (Position xP2 0.0)
        (player1 game)

newGame :: Time -> Game -> Game
newGame t game = changeTime t $ game { status = InsertPositionP1 }
------------------------------------------------

generateRandomPosition :: IO Position
generateRandomPosition = do
    newX <- newRandom >>= return . (/) 1 . fromIntegral
    newY <- newRandom >>= return . (/) 1 . fromIntegral 
    if validPosition (newX,newY)
        then return $ Position newX newY
        else generateRandomPosition

newRandom :: IO Integer
newRandom = Random.newStdGen >>= return . fst . Random.randomR (-100,100)

--------------------------------------------------------
