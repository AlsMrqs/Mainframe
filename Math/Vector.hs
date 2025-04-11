module Math.Vector where

    -- (Axis) Substantive --
data X = X deriving (Eq); instance Show X where show X = "x"
data Y = Y deriving (Eq); instance Show Y where show Y = "y"
data Z = Z deriving (Eq); instance Show Z where show Z = "z"

    {- Generic type -}
data Coordinate axis value = Coordinate axis value
    deriving (Eq)

instance (Show axis, Show value) => Show (Coordinate axis value) where
    show (Coordinate axis value) = show value ++ show axis

instance Functor (Coordinate axis) where
    fmap f (Coordinate axis value) = Coordinate axis (f value)

    {- Generic type -}
data Axis axis value = Axis [Coordinate axis value] 
    deriving (Eq, Show)

    {- Rigid type -}
data Vector = Vector (Coordinate X Double, Coordinate Y Double, Coordinate Z Double)
    deriving (Eq)

instance Show Vector where show (Vector v) = show v

