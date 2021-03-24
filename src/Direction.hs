module Direction where

-- Direction datatype declaration
data Direction
    = N
    | S
    | E
    | W
    deriving (Eq)

-- Show instance for Direction
instance Show Direction where
    show N = "north"
    show S = "south"
    show E = "east"
    show W = "west"
