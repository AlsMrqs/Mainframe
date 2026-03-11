module Struct.Shell where
import qualified Data.List as List (init, intercalate, reverse, uncons)
import qualified GHC.Int as GHC.Int

data ShellSize = ShellSize
    { widht  :: GHC.Int.Int32
    , height :: GHC.Int.Int32 } deriving Show

data Shell = Shell
    { size    :: ShellSize
    , history :: [String]
    , inbox   :: [Char] } -- deriving (Show)

instance Show Shell where
    show (Shell _ h i) = (List.intercalate "\n" $ List.reverse h) 
        ++ "\nHistory:"
        ++ "\nInbox: " ++ (show i)

main :: IO Shell
main = return newShell

newShell :: Shell
newShell = Shell (ShellSize (3+12) (5+12)) [] []

insertInbox :: Char -> Shell -> Shell
insertInbox c shell = let newInbox = (inbox shell) ++ [c] in
    case c of
        '\b' -> eraseInbox shell
        '\r' -> (deleteInbox . updateHistory) shell
        _    -> shell { inbox = newInbox }

eraseInbox :: Shell -> Shell
eraseInbox shell = shell { inbox = newInbox }
    where
    newInbox = if null (inbox shell) then [] else List.init (inbox shell)

deleteInbox :: Shell -> Shell
deleteInbox shell = shell { inbox = [] }

updateHistory :: Shell -> Shell
updateHistory shell = shell { history = newHistory }
    where
    newHistory = if null (inbox shell) 
        then (history shell)
        else (inbox shell) : (history shell)

lastInput :: Shell -> String
lastInput = maybe [] fst . (List.uncons . history)

