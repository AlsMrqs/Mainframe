module Space where

data Axis = Axis
    { lower  :: Float
    , higher :: Float } deriving Show

data Sense = Positive | Negative
    deriving Show

unit :: Axis -> Float
unit axis = (\x -> x * 1e-1) $ abs (lower axis) + (higher axis)

move :: Sense -> Axis -> Axis
move sense axis = let i = unit axis in case sense of
    Positive -> Axis (lower axis + i) (higher axis + i)
    Negative -> Axis (lower axis - i) (higher axis - i)

-- todo
data Space = Space 
    { x :: Axis
    , y :: Axis
    , z :: Axis } deriving Show

mainSpace = Space standard standard standard
standard = Axis (-0.5) 0.5


