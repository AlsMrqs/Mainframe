module Math.Vector where

-- singleton (a) -- Phanton type
data Axis a = Axis a (Float, Float)

class (Axis a) => Space a where
    x :: a -> Axis
    y :: a -> Axis
    z :: a -> Axis

class Axis a where
    limit :: a -> Float
    step :: (Num b) => a -> (b -> Float)

range :: (Axis a) => a -> [Float]
range axis = [-(limit axis), step(-(limit axis)) .. limit axis]

move :: Direction -> Space -> Axis
move dir space = case dir of
    Left  -> fmap (-1) (x axis)
    Right -> fmap (+1) (x axis)
    Up    -> fmap (+1) (y axis)
    Down  -> fmap (-1) (y axis)
    Front -> fmap (+1) (z axis)
    Back  -> fmap (-1) (z axis)

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

