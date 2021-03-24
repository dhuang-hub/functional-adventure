module Room where

import Data.List
import Item
import Direction

type RoomName = String
type Exit = (Direction, RoomName)


-- Room datatype declaration
data Room
    = Room { rname :: RoomName
           , desc :: String
           , exits :: [Exit]
           , objects :: [ItemName]}
    deriving (Show, Eq)


-- updates the objects in the room with an additional object
addItem :: ItemName -> Room -> Room
addItem item room = room { objects = item : (objects room) }


-- remove the first occurence of an item in an a room
removeItem :: ItemName -> Room -> Room
removeItem item room = room { objects = delete item (objects room) }


-- helper function for checking inventory of objects in a room
hasObjects :: Room -> Bool
hasObjects = (/= []) . objects