module Struct.System where
import qualified Graphics.UI.GLUT as GLUT
-- import qualified Struct.Program.Shell.Callback as Shell
import qualified Struct.Shell as Shell
import qualified Struct.Program.BitMap.Callback as BitMap
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
    -- { shell  :: Maybe Shell.Shell
    { bitmap :: Maybe BitMap.BitMap 
    } deriving Show

type ProgName = String

programs :: [ProgName]
programs = ["shell","bitmap"]

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
    "bitmap" -> \prg -> prg { bitmap = Nothing }
    _        -> id

startBitMap :: System -> System
startBitMap sys = sys { program = (startIt . program) sys }
    where
    startIt prog = prog { bitmap = Just ((BitMap.bitmap . size) sys) }

shellFunction :: (Shell.Shell -> Shell.Shell) -> System -> System
-- shellFunction f sys = Bool.bool sys sys' $ (Manager.isElem "shell" . manager) sys
--     where
--     sys' = sys { program = (shellApply f . program) sys } 
--     shellApply g prog = prog { shell = (fmap g . shell) prog }

shellFunction f sys = sys { shell = (f . shell) sys }

bitmapFunction :: (BitMap.BitMap -> BitMap.BitMap) -> System -> System
bitmapFunction f sys = Bool.bool sys sys' $ (Manager.isElem "bitmap" . manager) sys
    where
    sys' = sys { program = (bitmapApply f . program) sys } 
    bitmapApply g prog = prog { bitmap = (fmap g . bitmap) prog }

