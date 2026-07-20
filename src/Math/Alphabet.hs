module Math.Alphabet where
data Type = Starter_ | Separator_ | Finisher_ 
    | ParenthesisOpen_ | ParenthesisClose_
    | BracketOpen_ | BracketClose_
    | BracesOpen_ | BracesClose_
    | Punctuation_ 
    | Operator_
    | Variable_ 
    | Integer_ | Double_ | None_ 
    deriving (Show, Eq)
    
starter     :: [Char]
separator   :: [Char]
finisher    :: [Char]
punctuation :: [Char]
operator    :: [Char]
variable    :: [Char]
integer     :: [Char]
notation    :: [Char]

starter     = ['@'] -- ,'[','{']

isEnclose :: String -> Bool
isEnclose x = elem x ["(","[","{"]

parenthesisOpen = ['(']
parenthesisClose = [')']
bracketOpen = ['[']
bracketClose = [']']
bracesOpen = ['{']
bracesClose = ['}']

separator   = [',']
finisher    = ['@'] -- ,']','}']

punctuation = ['.']
operator    = ['+','-','*','/','^','#','!']
variable    = '_' : (['a'..'z'] ++ ['A'..'Z'])
integer     = ['0'..'9']
notation    = ['e','E']

token :: Char -> Type
token k
    | elem k starter     = Starter_
    | elem k parenthesisOpen  = ParenthesisOpen_
    | elem k parenthesisClose = ParenthesisClose_
    | elem k bracketOpen      = BracketOpen_
    | elem k bracketClose     = BracketClose_
    | elem k bracesOpen       = BracesOpen_
    | elem k bracesClose      = BracesClose_
    | elem k separator   = Separator_
    | elem k finisher    = Finisher_
    | elem k punctuation = Punctuation_
    | elem k operator    = Operator_
    | elem k variable    = Variable_
    | elem k integer     = Integer_
    | otherwise          = None_

