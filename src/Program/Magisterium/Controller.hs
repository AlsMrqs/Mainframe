module Program.Magisterium.Controller where

import Program.Magisterium.Magisterium
import qualified Data.Bool as Bool
import qualified System.Random as Random

newGameMsg   = "Shift player for a new round -> <- ?"
randomPosMsg = "Random position inserted!"
launchMsg    = "Launch time is over!"
trackMsg     = "Track time is over!"
blockMsg     = "Block time is over!"

timeOver :: Position -> Time -> Game -> Maybe (Message,Game)
timeOver randPos t game = do
    case status game of
        InsertPositionP1 -> event t 15 (randomPosMsg,insertRandomPositionP1 randPos) game
        InsertPositionP2 -> event t 15 (randomPosMsg,insertRandomPositionP2 randPos) game
        Launch           -> event t 120 (launchMsg,newGame) game
        Track            -> event t 60 (trackMsg,newGame) game
        Block            -> event t 60 (blockMsg,newGame) game
        Waiting Pause    -> Nothing
        Waiting NewGame  -> event t 5 (newGameMsg,newGame) game

event :: Time -> Time -> (Message, Time -> Game -> Game) -> Game -> Maybe (Message,Game)
event t limit (msg,f) game = Bool.bool Nothing (Just (msg,newGame)) activated
    where 
    activated = toSecond (t - t0) >= limit
    t0 = time game
    newGame = f t game

insertRandomPositionP1 :: Position -> Time -> Game -> Game
insertRandomPositionP1 randPos t game = nextRound t $ game { player1 = newPlayer }
    where
    newPlayer = insertPosition (randPos) (player1 game)

insertRandomPositionP2 :: Position -> Time -> Game -> Game
insertRandomPositionP2 (Position xP2 yP2) t game = 
    nextRound t $ game { player2 = newPlayer }
    where
    xP1 = (x . position . player1) game
    newXP2 = Bool.bool xP2 (negate xP2) (sameSideYAxis xP1 xP2)
    newPlayer = insertPosition (Position newXP2 yP2) (player2 game)

newGame :: Time -> Game -> Game
newGame t game = changeTime t $ game { status = InsertPositionP1 }

generateRandomPosition :: IO Position
generateRandomPosition = do
    newX <- newRandom >>= return . (*) 1e-2 . fromIntegral
    newY <- newRandom >>= return . (*) 1e-2 . fromIntegral 
    if validPosition (newX,newY)
        then return $ Position (roundTo3 newX) (roundTo3 newY)
        else generateRandomPosition

newRandom :: IO Integer
newRandom = Random.newStdGen >>= return . fst . Random.randomR (-999,999)

