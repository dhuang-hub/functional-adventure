module GameState where

import Data.List
import Control.Exception
import qualified Data.Map as Map

import Item
import Room
import Player
import Direction

type GameMap = Map.Map RoomName Room


-- GameState datatype declaration
data GameState
    = GameState { message :: Maybe String
                , gmap :: GameMap
                , universe :: Universe
                , player :: Player }
    deriving Show


-- make a gamemap
mkMap :: [Room] -> GameMap
mkMap someRooms = Map.fromList associationList
  where
    associationList = map (\room -> (rname room, room)) someRooms


-- Exception for lookup errors
data KeyError = KeyError
  deriving Show

instance Exception KeyError


-- error handling
getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case Map.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

-- Get object
getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)


-- get room from gamemap
getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case Map.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

-- getroom from gamestate
getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)


-- Update gameMap with a new room
setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap rname room gamemap = Map.insert rname room gamemap


-- Update game state message
setMessage :: String -> GameState -> GameState
setMessage str gstate = gstate { message = Just str }


-- Get the current inventory
currentInventory :: GameState -> [ItemName]
currentInventory gstate = inventory $ player gstate


-- Get current room of player
currentRoom :: GameState -> Room
currentRoom gstate = getRoom currRoomName gstate
  where currRoomName = location $ player gstate


-- Get objects of the room player is currently in
nearbyObjects :: GameState -> [ItemName]
nearbyObjects gstate = objects $ currentRoom gstate


-- player action - taking an item from the current room
takeItem' :: ItemName -> GameState -> GameState
takeItem' item gstate =
    let setRoom gs = gs { gmap = setRoomMap roomName newRoom gameMap }
        setPlayer gs = gs { player = newPlayer }
        setMsg gs = setMessage ("You take the " ++ item ++ ".") gs
    in setRoom . setPlayer . setMsg $ gstate
  where
    newPlayer = Player.addItem item $ player gstate
    newRoom = Room.removeItem item $ currentRoom gstate
    roomName = rname newRoom
    gameMap = gmap gstate


-- player action - dropping an item into the current room
dropItem' :: ItemName -> GameState -> GameState
dropItem' item gstate =
    let setRoom gs = gs { gmap = setRoomMap roomName newRoom gameMap }
        setPlayer gs = gs { player = newPlayer }
        setMsg gs = setMessage ("You drop the " ++ item ++ ".") gs
    in setRoom . setPlayer . setMsg $ gstate
  where
    newPlayer = Player.removeItem item $ player gstate
    newRoom = Room.addItem item $ currentRoom gstate
    roomName = rname newRoom
    gameMap = gmap gstate


-- player action - look at item either in inventory or nearby, unsafe function
lookAtItem' :: ItemName -> GameState -> GameState
lookAtItem' item gstate =
    setMessage (Item.desc . (getObject item) $ gstate) gstate



-- check on existence of an item in the inventory
inInventory :: ItemName -> GameState -> Error GameState
inInventory item gstate = case elem item $ currentInventory gstate of
    False -> Left errorMsg
    True -> Right gstate
  where
    errorMsg = "You don't have an " ++ item ++ " to use!"




-- get player inventory total weight
inventoryWeight :: GameState -> Integer
inventoryWeight gstate = foldr sumWeight 0 inv
  where
    sumWeight a b = (weight $ getObject a gstate) + b
    inv = currentInventory gstate


-- Error type declaration
type Error a = Either String a

-- Checks whether or not player already has item or not
alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck iname gstate
    | alreadyHave == True = Left errorMsg
    | otherwise = Right gstate
  where
    alreadyHave = elem iname $ currentInventory gstate
    errorMsg = "You are already carrying the  " ++ iname ++ "."


-- Checks whether or not an item is nearby
inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck iname gstate
    | inRoom == False = Left errorMsg
    | otherwise = Right gstate
  where
    inRoom = elem iname $ nearbyObjects gstate
    errorMsg = "There is no " ++ iname ++ " in this room."


-- Checks whether or not additional item exceeds weight limit
weightCheck :: ItemName -> GameState -> Error GameState
weightCheck iname gstate
    | overWeight == True = Left errorMsg
    | otherwise = Right gstate
  where
    overWeight = (inventoryWeight gstate + objWeight)
                 > (maxWeight . player $ gstate)
    objWeight = weight $ getObject iname gstate
    errorMsg = knowledgeCheck iname gstate


-- A helper function for weightCheck in producing a knowledge hint
knowledgeCheck :: ItemName -> GameState -> String
knowledgeCheck iname gstate = case iname of
    "knowledge" -> if (maxWeight $ player gstate) < 500 then
                   "Hmm... This is too much knowledge to simply take.\
                   \ I wonder if there are any resources around to use that\
                   \ can help me take in this enourmous knowledge...?"
                   else "That's too much weight for you to carry."
    otherwise -> "That's too much weight for you to carry."


-- Checks whether or not item is nearby or in inventory
anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck iname gstate
    | anywhere == False = Left errorMsg
    | otherwise = Right gstate
  where
    anywhere = or [nearby, inventory]
    nearby = elem iname $ nearbyObjects gstate
    inventory = elem iname $ currentInventory gstate
    errorMsg = "What do you mean, drop the \"" ++ iname ++ "\"?"


-- Checks whether or not an item is in the current room
inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck iname gstate
    | inRoom == True = Left errorMsg
    | otherwise = Right gstate
  where
    inRoom = elem iname $ nearbyObjects gstate
    errorMsg = "You aren't carrying the " ++ iname ++ "."


-- Checks for objects in the current room of a gamestate
roomHasObjects :: GameState -> Bool
roomHasObjects = hasObjects . currentRoom


-- An improved error-proof takeItem function
takeItem :: ItemName -> GameState -> GameState
takeItem item gstate =
  let checks a b = alreadyHaveTakeCheck a b >> inRoomTakeCheck a b
                   >> weightCheck a b
  in case checks item gstate of
    Left msg -> setMessage msg gstate
    Right _ -> takeItem' item gstate


-- An improved error-proof dropItem function
dropItem :: ItemName -> GameState -> GameState
dropItem item gstate =
  let checks a b = anywhereDropCheck a b >> inRoomDropCheck a b
  in case checks item gstate of
    Left msg -> setMessage msg gstate
    Right _ -> dropItem' item gstate


-- Error-proof lookAt function
lookAtItem :: ItemName -> GameState -> GameState
lookAtItem item gstate =
  let near = elem item $ nearbyObjects gstate
      inv = elem item $ currentInventory gstate
  in case near || inv of
    False -> setMessage err gstate
    True -> lookAtItem' item gstate
  where err = "There's no \"" ++ item ++ "\" to look at!"


-- player action of using item
-- if an object has a 'False' use flag, it returns the object's desc.
useItem :: ItemName -> GameState -> GameState
useItem item gstate =
  let checks a b = useItemCheck a b >> canUseCheck a b
  in case checks item gstate of
    Left msg -> setMessage msg gstate
    Right _ -> useItem' item gstate
  where
    canUseCheck item gstate = case usable $ getObject item gstate of
      False -> Left $ (Item.desc $ getObject item gstate)
      True -> Right gstate


-- error-pipeline for use item
useItemCheck :: ItemName -> GameState -> Error GameState
useItemCheck item gstate =
  let near = elem item $ nearbyObjects gstate
      inv = elem item $ currentInventory gstate
  in case near || inv of
    False -> Left err
    True -> Right gstate
  where err = "There's no \"" ++ item ++ "\" to use!"


-- player action - use an item in inventory, branching of 'useable' items
-- unsafe function
useItem' :: ItemName -> GameState -> GameState
useItem' item gs = case item of
  "book" -> useBook gs
  "computer" -> useComputer gs
  "basementdoor" -> useBasementDoor gs
  "maindoor" -> useMainDoor gs
  "floortwodoor" -> useFl2Door gs
  "floorthreedoor" -> useFl3Door gs
  "knowledge" -> setMessage "I'm... feeling... kind.. of... powerful!" gs
  "stairsup" ->  useStairsUp gs
  "stairsdown" -> useStairsDown gs


-- use book player actionm, adds to player's max weight
useBook :: GameState -> GameState
useBook gstate = 
  setMessage "Wow! I think I'm beginning to succeed at Haskell!" newState
  where
    newWeight = 250 + (maxWeight $ player gstate)
    newPlayer = (player gstate) { maxWeight = newWeight }
    newState = gstate { player = newPlayer }


-- use computer player actionm, adds to player's max weight
useComputer :: GameState -> GameState
useComputer gstate =
  setMessage "Wow! There is A LOT of good in Haskell." newState
  where
    newWeight = 250 + (maxWeight $ player gstate)
    newPlayer = (player gstate) { maxWeight = newWeight }
    newState = gstate { player = newPlayer }


-- use basement door player action
useBasementDoor :: GameState -> GameState
useBasementDoor gstate = case "key" `elem` currentInventory gstate of
  False -> setMessage "This door is locked! I need a key..." gstate
  True -> setMessage "Oh goodie, a key that works! You go East." newState
  where
    newState = case rname $ currentRoom gstate of
      "Basement Stairwell" -> gstate { player = newPlayer "Basement Room" }
      "Basement Room" -> gstate { player = newPlayer "Basement Stairwell" }
    newPlayer dest = (player gstate) { location = dest }

  
-- use main door player action
useMainDoor :: GameState -> GameState
useMainDoor gstate = case "knowledge" `elem` currentInventory gstate of
  False -> setMessage "This door is locked! Huh... Someone left a note." gstate
  True -> setMessage "This knowledge is setting me free!" newState
  where
    newState = gstate { player = newPlayer }
    newPlayer = (player gstate) { location = "End" }


-- use floor 2 door action, this door never opens
useFl2Door :: GameState -> GameState
useFl2Door gstate = case "badge" `elem` currentInventory gstate of
  False -> setMessage
           "This door is locked! I need to swipe in somehow..." gstate
  True -> setMessage "Shoot! I think this door's scanner is faulty...\
                     \ I wonder if there is another one." gstate


-- use floor 3 door action, door opens if player has a "badge"
useFl3Door :: GameState -> GameState
useFl3Door gstate = case "badge" `elem` currentInventory gstate of
  False -> setMessage
           "This door is locked! I need to swipe in somehow..." gstate
  True -> setMessage
           "Took a few swipes, but this badge opened the door!\
           \ You go East." newState
  where
    newState = case rname $ currentRoom gstate of
      "Floor 3 Stairwell" -> gstate { player = newPlayer "Floor 3 Entrance" }
      "Floor 3 Entrance" -> gstate { player = newPlayer "Floor 3 Stairwell" }
    newPlayer dest = (player gstate) { location = dest }


-- use a flight of stairs heading up
useStairsUp :: GameState -> GameState
useStairsUp gstate = setMessage "Huff... huff... so many steps..." newState
  where
    newState = case rname $ currentRoom gstate of
        "Basement Stairwell" ->  gstate { player = newPlayer "Floor 1 Stairwell" }
        "Floor 1 Stairwell" -> gstate { player = newPlayer "Floor 2 Stairwell" }
        "Floor 2 Stairwell" -> gstate { player = newPlayer "Floor 3 Stairwell" }
    newPlayer dest = (player gstate) { location = dest }


-- use a flight of stairs heading down
useStairsDown :: GameState -> GameState
useStairsDown gstate = setMessage "Step - step - step on down!" newState
  where
    newState = case rname $ currentRoom gstate of
        "Floor 1 Stairwell" ->  gstate { player = newPlayer "Basement Stairwell" }
        "Floor 2 Stairwell" -> gstate { player = newPlayer "Floor 1 Stairwell" }
        "Floor 3 Stairwell" -> gstate { player = newPlayer "Floor 2 Stairwell" }
    newPlayer dest = (player gstate) { location = dest }


-- Function for determining the destination of a given direction
destinationName :: Direction -> Room -> Maybe RoomName
destinationName dir room = lookup dir $ exits room


-- Function for moving the player in a given direction
move :: Direction -> GameState -> GameState
move dir gstate = case destinationName dir $ currentRoom gstate of
    Just dest ->
      setMessage moveMsg $ gstate { player = updatePlayerLocation dest }
    otherwise -> setMessage "There is no exit in that direction." gstate
  where
    moveMsg =  "You go " ++ show dir ++ "."
    updatePlayerLocation dest = (player gstate) { location = dest }


-- function for determining player winning
-- player gains knowledge and gets past 'maindoor'
haveWonGame :: GameState -> Bool
haveWonGame gstate = (rname $ currentRoom gstate) == "End"