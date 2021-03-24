module Item where
import qualified Data.Map as Map


type ItemName = String
type Universe = Map.Map String Item


-- Item datatype declaration
data Item
    = Item { iname :: ItemName
           , desc :: String
           , weight :: Integer
           , usable :: Bool }
    deriving  (Show, Eq)


-- make a universe of items from a list of items
mkUniverse :: [Item] -> Universe
mkUniverse someItems = Map.fromList associationList
  where
    associationList = map (\item -> (iname item, item)) someItems