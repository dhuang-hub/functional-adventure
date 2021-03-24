module Initialize where

import qualified Data.Map as Map

import Room
import Item
import Direction
import Player
import GameState

-- Item Initialization
couch :: Item
couch = Item "couch"
             "This couch is heavy, worn, and streaked with brown...?"
             50
             False


stapler :: Item
stapler = Item "stapler"
               "CLICK-CLACK. Oh... It's a stapler."
               8
               False


whiteboard :: Item
whiteboard = Item "whiteboard"
                  "\"Borja wuz hurrr\" Hmm... I wonder who wrote that?"
                  60
                  False


pot :: Item
pot = Item "pot"
           "Is this legal? Probably shouldn't use that inside a library..."
           1
           False


-- need to 'use' to increase maxWeight capacity
book :: Item
book = Item "book" 
            "Finding Success (and Failure) In Haskell - Moronuki\
            \ & Martin... Interesting."
            5
            True


-- need to 'use' to increase maxWeight capacity
computer :: Item
computer = Item "computer"
                "Slick Macs with huge monitors! Hmm... looks like someone was\
                \ reading \"Learn You a Haskell for Great Good!\" Neat."
                99
                True


-- need to have in inventory in order to enter basement room
key :: Item
key = Item "key"
           "Looks like a key that could be used to open locked doors?"
           5
           False


-- need to have in inventory in order to enter floor 3
badge :: Item
badge = Item "badge"
             "Someone left their ID card! Who's David Huang?"
             5
             False


stairs_up :: Item
stairs_up = Item "stairsup"
                 "So many steps, looks exhausting."
                 999
                 True


stairs_down :: Item
stairs_down = Item "stairsdown"
                 "The way down should be easier."
                 999
                 True


basement_door :: Item
basement_door = Item "basementdoor"
                     "Darn this door is locked! What to do? I'm dying to know\
                     \ what's on the other side! Looks like I need a key."
                     999
                     True


main_door :: Item
main_door = Item "maindoor"
                     "I think the objective is to escape! Of course this\
                     \ door is locked... There's a note: Knowledge is \
                     \power."
                     999
                     True


fl2_door :: Item
fl2_door = Item "floortwodoor"
                 "Looks like a computer science department? I need\
                 \ a badge or ID card to scan for access."
                 999
                 True


fl3_door :: Item
fl3_door = Item "floorthreedoor"
                "Hmm... I wonder what I'll find on the other side of this\
                \ door?"
                999
                True


knowledge :: Item
knowledge = Item "knowledge"
                 "Whoah, this is SUPER FUNCTIONAL knowledge."
                 500
                 True



-- Room Initialization
basement_copy :: Room
basement_copy = Room rname desc exits objects
  where
    rname = "Copy Room"
    desc = "Where am I? It's just faulty office equipment junk. \
           \How do I get out of here?"
    exits = [(S,  "Stacks")]
    objects = ["stapler"]


basement_stacks :: Room
basement_stacks = Room rname desc exits objects
  where
    rname = "Stacks"
    desc = "Hmm... This place is creepy. I think I'm in a basement. I see\
           \ mechanical book stacks and a big glass room. Is this a library?"
    exits = [(N, "Copy Room"), (S, "Basement Stairwell"),
             (W, "Basement Office")]
    objects = []


basement_office :: Room
basement_office = Room rname desc exits objects
  where
    rname = "Basement Office"
    desc = rname ++ ": \nOh man! This looks like an office!"
    exits = [(E, "Stacks")]
    objects = ["stapler", "book"]


basement_stairs :: Room
basement_stairs = Room rname desc exits objects
  where
    rname = "Basement Stairwell"
    desc = rname ++ ": \nThere are stairs! They appear to lead up somewhere..."
    exits = [(N, "Stacks")]
    objects = ["stairsup", "basementdoor"]


basement_lecture :: Room
basement_lecture = Room rname desc exits objects
  where
    rname = "Basement Room"
    desc = rname ++ ": \nOMG. I just walked into a Functional Programming\
           \ Lecture!!! Taught by Matt Teichman! Now we're rockin'!"
    exits = [(W, "Basement Stairwell")]
    objects = ["knowledge"]


fl1_stairs :: Room
fl1_stairs = Room rname desc exits objects
  where
    rname = "Floor 1 Stairwell"
    desc = rname ++ ": \nWow! These stairs look so twisty. There are flights\
           \ of stairs leading up and leading down... I think this is the\
           \ first floor."
    exits = [(E, "Floor 1 Atrium")]
    objects = ["stairsup", "stairsdown"]

  
fl1_atrium :: Room
fl1_atrium = Room rname desc exits objects
  where
    rname = "Floor 1 Atrium"
    desc = rname ++ ": \nA spacious sunlit atrium. I think I see the\
           \ building's main entrance."
    exits = [(N, "Floor 1 Lab"), (W, "Floor 1 Stairwell")]
    objects = ["maindoor"]


fl1_lab :: Room
fl1_lab = Room rname desc exits objects
  where
    rname = "Floor 1 Lab"
    desc = rname ++ ":  \nComputers and technology as far as the eye can see.\
           \ Whoah, is that a Nintendo Switch?!"
    exits = [(S, "Floor 1 Atrium")]
    objects = ["computer", "badge"]


fl2_stairs :: Room
fl2_stairs = Room rname desc exits objects
  where
    rname = "Floor 2 Stairwell"
    desc = rname ++ ": \nOh my, these stairs are so narrow and twisty."
    exits = []
    objects = ["stairsdown", "stairsup", "floortwodoor"]


fl3_stairs :: Room
fl3_stairs = Room rname desc exits objects
  where
    rname = "Floor 3 Stairwell"
    desc = rname ++ ": \nNo more flights of stairs to climb. Thank goodness!"
    exits = []
    objects = ["stairsdown", "floorthreedoor"]


-- floor 3 entrance
fl3_entrance :: Room
fl3_entrance = Room rname desc exits objects
  where
    rname = "Floor 3 Entrance"
    desc = rname ++ ": \nNice cushy seats and whiteboards everywhere!"
    exits = [(N, "Floor 3 Atrium"), (W, "Floor 3 Stairwell")]
    objects = ["whiteboard"]


-- floor 3 atrium
fl3_atrium :: Room
fl3_atrium = Room rname desc exits objects
  where
    rname = "Floor 3 Atrium"
    desc = rname ++ ": \nWow, this place feels so familiar... I could've sworn\
           \ there should be another set of stairs here..."
    exits = [(N, "Floor 3 Offices"), (S, "Floor 3 Entrance")]
    objects = ["couch", "pot"]


-- floor 3 office
fl3_office :: Room
fl3_office = Room rname desc exits objects
  where 
    rname = "Floor 3 Offices"
    desc = rname ++ ": \nI'm so lost, I just circled and circled so many\
           \ glass-paneled offices. I ended up in... Gerry Brady's Office?"
    exits = [(N, "Floor 3 Atrium")]
    objects = ["key"]


-- Game ends upon arrival of this room
end_game_room :: Room
end_game_room = Room rname desc exits objects
  where 
    rname = "End"
    desc = ""
    exits = []
    objects = []


-- can't be carried
heavyItems :: [Item]
heavyItems = [ stairs_up, basement_door, fl2_door, fl3_door
             , main_door, stairs_down ]


-- can be carried
lightItems :: [Item]
lightItems = [ pot, book, computer, badge, key, knowledge, stapler
             , couch, whiteboard ]


-- Item Universe
univ :: Universe
univ = mkUniverse $ heavyItems ++ lightItems


-- Item name constants
itemNames :: [ItemName]
itemNames = Map.keys univ


-- All rooms
gameRooms :: [Room]
gameRooms = [basement_copy, basement_stacks, basement_stairs, basement_office,
             fl1_stairs, fl1_atrium, fl1_lab, fl2_stairs, fl3_stairs,
             fl3_entrance, fl3_office, fl3_atrium, basement_lecture,
             end_game_room]


-- Game Map Init.
gameMap :: GameMap
gameMap = mkMap gameRooms


-- Room name constants
roomNames :: [RoomName]
roomNames = Map.keys gameMap


-- Initialization player
you :: Player
you = Player
      []
      10
      "Copy Room"


-- Initial state creation
initialState :: GameState
initialState
  = GameState
    Nothing
    gameMap
    univ
    you