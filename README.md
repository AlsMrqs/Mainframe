# Mainframe
## Mainframe is a software for teaching physics and mathematics through gamification

### Functions

```haskell
lex :: [Char] -> (Token,[Char])

parse :: [Char] -> Either String (Tree Token)

type Point = (Double,Double,Double) -- (x,y,z) cartesian axis

solve' :: Tree Token -> Point -> Double
```

### Developing
- todo: insert (sin,cos,tan) fu
- Gameplay (projectile and analysis)
- Animation (cartesian results)

The parser will be rebuilt in the 'folklore' repository!

