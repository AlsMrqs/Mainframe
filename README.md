# Mainframe
## Mainframe is a software for teaching physics and mathematics through gamification

### Function - lexer
Receives a string and splits it into tokens recognized by the language.

```haskell
lexer :: [Char] -> ([Char], [Char])
lexer "x+1" = ("x","+1")
lexer  "+1" = ("+","1")
lexer   "1" = ("1","")
lexer "@naturefx" = ("","@naturefx")
```

### Function - parser
Receives an expression and checks whether it is syntatically correct.

```haskell
parser :: [Char] -> Either Error Bool
parser "(x+1,y^2,z-2)" = Right True
parser "(abcde)" = Left "Error msg"
```

### Developing
- Implementing the syntax tree structure
- Adding semantic analysis
- Building a REPL interface for testing expressions

