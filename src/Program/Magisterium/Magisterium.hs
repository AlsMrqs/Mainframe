module Program.Magisterium.Magisterium where

import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as GLUT
import qualified Math as Math
import qualified Screen as Screen
import qualified Space as Space

import qualified Math.Solver as Math.Solver
import qualified Math.Parser as Math.Parser

import qualified Control.Concurrent as Concurrent (forkIO, killThread, MVar, modifyMVar_,threadDelay)
import qualified Graph as Graph
import qualified Control.Monad.State as State
import qualified Data.Bool as Bool
import qualified Data.Map as Map
import qualified Text.Read as Read
import qualified System.Random as Random
import qualified Machine.Solve as Machine
-- import qualified Solve as Solve -- Convergence

-----------------{ Function } ------------------
newtype Function = Function { apply :: Double -> Double }

type Trail = [Space.Point]
type Time = Double

toSecond :: Time -> Time
toSecond = flip (/) 1e3

trace :: Function -> Time -> Time -> Maybe Trail
trace f t0 t = Bool.bool (return newStack) Nothing (dt > 2)
    where
    dt       = (t/100)-(t0/100)
    step     = negate 0.001
    newStack = foldl (flip (allocTrail f)) [] [1,1+step..1+(negate dt)]

trace' :: Function -> Time -> Time -> Maybe Trail
trace' f t0 t = Bool.bool (return newStack) Nothing ((dt*dx) > 2)
    where
    dt       = t-t0
    dx       = 2 / 60e3
    step     = negate 0.001
    newStack = foldl (flip (allocTrail f)) [] [1,1+step..(-) 1 (dx*dt)]

allocTrail :: Function -> Time -> Trail -> Trail
allocTrail f t stack = (point:stack)
    where
    point = (t,(apply f) t,0)

derive :: Function -> Function
derive func = Function f'
    where
    f  = apply func
    f' x = let h = 0.000001 in (f (x+h) - f x) / h

intersect :: Function -> Function
intersect f = Function p'
    where
    p' x = negate ((/) 1 (apply (derive f) x))

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

isPerpendicular :: Function -> Function -> Bool
isPerpendicular f g = fs == gs
    where
    fs = map (roundTo3 . \x -> (-1) / apply f x) range
    gs = map (roundTo3 . apply g) range

isDerivative :: Function -> Function -> Bool
isDerivative f g = f' == g'
    where
    f' = map (roundTo3 . apply (derive f)) range
    g' = map (roundTo3 . apply g) range

follow :: (GLUT.GLsizei,GLUT.GLsizei) -> Function -> Double -> [Space.Point]
follow (dx,dy) f x = map (Math.sumPoint (x, apply f x,0) .  flip Math.rotateZ alpha) 
        (squaredAim (dx,dy))
    where
    alpha = atan (apply (derive f) x)

crossAim :: (GLUT.GLsizei,GLUT.GLsizei) -> [Space.Point]
crossAim (x,y) = []

squaredAim :: (GLUT.GLsizei,GLUT.GLsizei) -> [Space.Point]
squaredAim (x,y) = lxUp ++ lxDown ++ lyUp ++ lyDown
    where
    lxUp   = [(negate xBorder, yBorder,0),(negate xDelta, yBorder,0)] ++ 
             [(xBorder, yBorder,0),(xDelta, yBorder,0)]
    lxDown = [(negate xBorder,negate yBorder,0),(negate xDelta,negate yBorder,0)] ++ 
             [(xBorder,negate yBorder,0),(xDelta,negate yBorder,0)]
    lyUp   = [(negate xBorder, yBorder,0),(negate xBorder, yDelta,0)] ++ 
             [(xBorder, yBorder,0),(xBorder, yDelta,0)]
    lyDown = [(negate xBorder,negate yBorder,0),(negate xBorder,negate yDelta,0)] ++ 
             [(xBorder,negate yBorder,0),(xBorder,negate yDelta,0)]

    xBorder = (*) 0.05 $ 300/(fromIntegral x)
    xDelta  = (*) 0.01 $ 300/(fromIntegral x)
    yBorder = (*) 0.05 $ 300/(fromIntegral y)
    yDelta  = (*) 0.01 $ 300/(fromIntegral y)

grid :: [Space.Point]
grid = xLines ++ yLines
    where
    steps = [-1,(-1)+2e-1..1]
    xLines = foldl (\acc k -> (-1,k,0):(1,k,0):acc) [] steps
    yLines = foldl (\acc k -> (k,-1,0):(k,1,0):acc) [] steps

aim :: [Space.Point]
aim = lxUp ++ lxDown ++ lyUp ++ lyDown
    where
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

data Round = A Offensive Defensive | B Defensive Offensive
    deriving Show

flipOrder :: Round -> Round
dlipOrder (A off def) = B def off
flipOrder (B def off) = A off def

offensive :: Round -> Offensive
offensive order = case order of
    A off _ -> off
    B _ off -> off

defensive :: Round -> Defensive
defensive order = case order of
    A _ def -> def
    B def _ -> def

insertOffensive :: Time -> Function -> Round -> Round
insertOffensive  t0 f order = let newProjectile = newOffensive t0 f in
    case order of
        A off def -> A newProjectile def
        B def off -> B def newProjectile 

insertDefensive :: Function -> Round -> Round
insertDefensive f order = let newAnalysis = newDefensive f in
    case order of
        A off def -> A off newAnalysis
        B def off -> B newAnalysis off

data Game = Game 
    { scale   :: Scale 
    , time    :: Time
    , status  :: Status
    , currentRound :: Round
    , player1 :: Player
    , player2 :: Player }

changeTime :: Time -> Game -> Game
changeTime t0 game = game { time = t0 }

isWaiting :: Game -> Bool
isWaiting game = case status game of
    Waiting _ -> True
    _         -> False

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

positionP1 :: Game -> Position
positionP1 = position . player1

positionP2 :: Game -> Position
positionP2 = position . player2

insertName :: String -> Player -> Player
insertName str player = player { nickName = str }

insertPosition :: Position -> Player -> Player
insertPosition newPos player = player { position = newPos }

offensivePlayer :: Game -> Player
offensivePlayer game = case currentRound game of
    A off _ -> player1 game
    _       -> player2 game

defensivePlayer :: Game -> Player
defensivePlayer game = case currentRound game of
    A _ def -> player2 game
    _       -> player1 game

--------------------{ GameRunTime }-----------------------

nextRound :: Time -> Game -> Game
nextRound t0 game = case status game of
    InsertPositionP1 -> changeTime t0 $ game { status = InsertPositionP2 }
    InsertPositionP2 -> changeTime t0 $ game { status = Launch }
    Launch           -> changeTime t0 $ game { status = Track }
    Track            -> game { status = Block }
    Block            -> changeTime t0 $ game { status = Waiting NewGame }
    Waiting NewGame  -> changeTime t0 $ game { status = InsertPositionP1 }

selectRound :: Status -> Game -> Game
selectRound newStatus game = game { status = newStatus }

type Input = String
type Error = String
type Message = String
type StateComputation = State.StateT Game (Either Error) Message

play :: Time -> String -> Game -> IO (Either Error (String,Game))
play t str game = case status game of
    InsertPositionP1 -> return $ readPoint str    
        >>= pure . fmap (nextRound t) . flip insertPositionP1 game
    InsertPositionP2 -> return $ readPoint str    
        >>= fmap (fmap (nextRound t)) . flip insertPositionP2 game
    Launch ->  do -- MACHINE INPUT (!!)
        newStr <- if str == " "
            then do
                degree <- Machine.randomDegree
                let p1 = tuplePosition . positionP1 $ game
                let p2 = tuplePosition . positionP2 $ game
                Machine.solution degree p1 p2
            else return str
        return $ readFunction newStr -- str 
                        >>= fmap (fmap (nextRound t)) . flip (launch t) game
    Track  -> return $ readFunction str 
        >>= fmap (fmap (nextRound t)) . flip track game
    Block  -> return $ readFunction str 
        >>= fmap (fmap (nextRound t)) . flip block game
    Waiting NewGame -> do
        return $ 
            if str == "continue"
                then pure . (,) "NewGame Starting!" $ nextRound t game
                else Left "Type \"continue\" to start a new game!"
    _                -> return $ pure (str,game)

-- play :: Time -> String -> Game -> Either Error (String,Game)
-- play t str game = case status game of
--     InsertPositionP1 -> readPoint str    
--         >>= pure . fmap (nextRound t) . flip insertPositionP1 game
--     InsertPositionP2 -> readPoint str    
--         >>= fmap (fmap (nextRound t)) . flip insertPositionP2 game
--     Launch ->  do -- MACHINE INPUT (!!)
--         -- if null str 
--         --     then 
--         readFunction str >>= fmap (fmap (nextRound t)) . flip (launch t) game
--     Track  -> readFunction str >>= fmap (fmap (nextRound t)) . flip track game
--     Block  -> readFunction str >>= fmap (fmap (nextRound t)) . flip block game
--     Waiting NewGame -> 
--         if str == "continue"
--             then pure . (,) "NewGame Starting!" $ nextRound t game
--             else Left "Type \"continue\" to start a new game!"
--     _                -> pure (str,game)

---------------------{ Player Positioning }---------------------todo!!!!!!
launch :: Time -> Function -> Game -> Either Error (String,Game)
launch t0 f game = 
    let (xOff,yOff) = (tuplePosition . position . offensivePlayer) game
        (xDef,yDef) = (tuplePosition . position . defensivePlayer) game
        origin      = abs ((roundTo3 $ apply f xOff) - yOff) < 1e-9
        hit         = abs ((roundTo3 $ apply f xDef) - yDef) < 1e-9
        newOrder    = (insertOffensive t0 f . currentRound) game
    in if not hit || not origin
        then Left "Invalid projectile!"
        else pure . (,) "Expression accepted!" . changeTime t0 
            $ game { currentRound = newOrder }

track :: Function -> Game -> Either Error (String,Game)
track f game = 
    let newOrder = (insertDefensive f . currentRound) game
        launcher = (projectile . offensive . currentRound) game
    in case launcher of
        Nothing          -> pure . (,) "Nothing to track! Restarting!" 
            $ selectRound Launch game
        Just (_,project) ->
            if (not . isDerivative project) f
                then Left "Untracked projectile!"
                else pure . (,) "Projectile Tracked!" $ game { currentRound = newOrder }

block :: Function -> Game -> Either Error (String,Game)
block f game =
    let tracker = (analysis . defensive . currentRound) game
    in case tracker of
        Nothing -> pure . (,) "Can't block without track object!"
            $ selectRound Track game
        Just f' -> 
            if (not . isPerpendicular f') f
                then Left "Not blocked!"
                else pure . (,) "Projectile Blocked!" $ game
            
---------------------------------------------------------------

----------- Parser -------------
-- readFunction :: Input -> Either Error Function 
-- readFunction str = fmap (Function . f) (Grammar.parse str)
-- h
--     where
--     f tree x = Solver.solve' tree (x,0,0)

-- newtype Function = Function { apply :: Double -> Double }

readFunction :: Input -> Either Error Function
readFunction str = do
    let dict x   = Map.fromList $ zip ["x","y","z"] ([x,0,0]::[Double])
        errorMsg = "Can't build this function!"
    Math.Parser.parse str 
        >>= \ast -> Math.Solver.solve (dict 0) ast
        >>= \_   -> return 
            $ Function (\x -> either (const 0) id (Math.Solver.solve (dict x) ast))
--------------------------------

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

roundTo2 :: Double -> Double
roundTo2 x = fromIntegral (round (x * 1e2)) / 1e2

roundTo3 :: Double -> Double
roundTo3 x = fromIntegral (round (x * 1e2)) / 1e2

