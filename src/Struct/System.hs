module Struct.System where
import qualified Graphics.UI.GLUT as GLUT
-- import qualified Struct.Program.Shell.Callback as Shell
import qualified Struct.Shell as Shell
import qualified Struct.Program.BitMap.Callback as BitMap
import qualified Struct.Program.Magisterium.Callback as Magisterium -- todo!!!!
import qualified Struct.Manager as Manager
import qualified Struct.Graph as Graph
import qualified Data.List as List (elem)
import qualified Data.Bool as Bool (bool)
import qualified Data.Maybe as Maybe (isNothing)

data System = System 
    { size    :: GLUT.Size
    , shell   :: Shell.Shell
    , manager :: (Manager.Manager String) 
    , program :: Program 
    } deriving Show

data Program = Program
    { bitmap      :: Maybe BitMap.BitMap 
    -- , magisterium :: Maybe Magisterium.Magisterium
    , derivative  :: Maybe Magisterium.Game
    } deriving Show

-- callMagisterium = magisterium . program
callBitmap      = bitmap . program
callDerivative  = derivative . program

type ProgName = String

programs :: [ProgName]
programs = ["bitmap","derivative"]

-- System Functions

currentProgram :: System -> ProgName
currentProgram = maybe "" Graph.vert . manager

startProgram :: String -> System -> Either String System
startProgram str sys
    | (not . List.elem str) programs     = Left "Don't a program!"
    | (Manager.isElem str . manager) sys = Left "Already running!"
    | otherwise                          = Right 
        $ sys { manager = Manager.insert str (manager sys) }

selectProgram :: String -> System -> Either String System
selectProgram str sys
    | (Maybe.isNothing . Manager.select str . manager) sys = Left "Not running!"
    | otherwise                                            = Right 
        $ sys { manager = Manager.select str (manager sys) }

killProgram :: String -> System -> Either String System
killProgram str sys
    | (not . Manager.isElem str . manager) sys = Left "Not running!"
    | otherwise                                = Right 
        $ (\sys' -> sys' { program = (close str . program) sys' })
        $ sys { manager = (Manager.remove str . manager) sys }
        -- update (program)

nextProgram :: System -> System
nextProgram sys = sys { manager = (Manager.next . manager) sys }

prevProgram :: System -> System
prevProgram sys = sys { manager = (Manager.prev . manager) sys }

modifier :: Char -> (System -> System)
modifier k = case k of
    '+' -> nextProgram
    '-' -> prevProgram
    _ -> id

-- Programs Functions

close :: String -> (Program -> Program)
close str = case str of
    "bitmap"      -> \prg -> prg { bitmap = Nothing }
    "derivative"  -> \prg -> prg { derivative = Nothing }
    -- "magisterium" -> \prg -> prg { magisterium = Nothing }
    _        -> id

startBitMap :: System -> System
startBitMap sys = sys { program = (startIt . program) sys }
    where
    startIt prog = prog { bitmap = Just ((BitMap.bitmap . size) sys) }

shellFunction :: (Shell.Shell -> Shell.Shell) -> System -> System
shellFunction f sys = sys { shell = (f . shell) sys }

-- magisteriumFunction :: (Magisterium.Magisterium -> Magisterium.Magisterium) -> System -> System
-- magisteriumFunction f sys = Bool.bool sys sys' $ (Manager.isElem "magisterium" . manager) sys
--     where
--     sys' = sys { program = (magisteriumApply f . program) sys } 
--     magisteriumApply g prog = prog { magisterium = (fmap g . magisterium) prog }

bitmapFunction :: (BitMap.BitMap -> BitMap.BitMap) -> System -> System
bitmapFunction f sys = Bool.bool sys sys' $ (Manager.isElem "bitmap" . manager) sys
    where
    sys' = sys { program = (bitmapApply f . program) sys } 
    bitmapApply g prog = prog { bitmap = (fmap g . bitmap) prog }

derivativeFunction :: (Magisterium.Game -> Magisterium.Game) -> System -> System
derivativeFunction f sys = Bool.bool sys sys' $ (Manager.isElem "derivative" . manager) sys
    where
    sys' = sys { program = (derivativeApply f . program) sys }
    derivativeApply g prog = prog { derivative = (fmap g . derivative) prog }

