module Engine.Editor where

import Data.Map

newtype Text = Text (Map Int [Char])

data Editor   = Editor { cursor :: Cursor, text ::  Text }
data Cursor   = Cursor { mode :: Mode, location :: Location }
data Location = Location { line :: Int, row :: Int }

data Mode = Insertion | Command
    deriving Show

shift :: Mode -> Mode
shift x = case x of
    Insertion -> Command
    _         -> Insertion

shiftCursorMode :: Editor -> Editor
shiftCursorMode (Editor c t) = Editor (f c) t
    where
    f (Cursor m l) = Cursor (shift m) l 


