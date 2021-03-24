module Player where
import Data.List
import Item
import Room


-- Player datatype declaration
data Player
    = Player { inventory :: [ItemName]
             , maxWeight :: Integer
             , location :: RoomName }
    deriving (Show, Eq)


-- update player's inventory with new additional item
addItem :: ItemName -> Player -> Player
addItem item player = player { inventory = item : (inventory player) }


-- remove a single appearance of an item from inventory
removeItem :: ItemName -> Player -> Player
removeItem item player = player { inventory = delete item (inventory player) }