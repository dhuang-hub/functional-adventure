module Command where

import Data.Char
import Data.List
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (optional)

import Item
import Direction


-- renaming ReadP's record field to what we had
runParser :: ReadP a -> String -> [(a, String)]
runParser = readP_to_S


-- Command data declaration
data Command
  = Inventory
  | Look
  | LookAt [ItemName]
  | Drop [ItemName]
  | Take [ItemName]
  | Use [ItemName]
  | Move Direction
  | Exit
  | Help
  deriving (Eq, Show)


-- type synonym for lists of commands
type Conjunction = [Command]


-- turns a parser into a parser that removes whitespace from both sides
whitespacify :: ReadP a -> ReadP a
whitespacify prsr = skipSpaces
                    *> prsr
                    <* skipSpaces


-- helper bool function for checking conditions of alpha AND lower characters
isAlphaLower :: Char -> Bool
isAlphaLower a = isAlpha a && isLower a


-- a parser for alpha-lowercase nouns
nounPhrase_stub :: ReadP [ItemName]
nounPhrase_stub = whitespacify $ pure (\i -> [i])
                  <*> munch1 isAlphaLower


-- a parser for phrases of nouns
nounPhrase :: ReadP [ItemName]
nounPhrase = (pure (\xs -> [x' | x <- xs, x' <- x])
             <*> nounPhrase_stub `sepBy` (char ',')
             <* eof)
             <++
             (pure (\xs -> [x' | x <- xs, x' <- x])
             <*> nounPhrase_stub `sepBy` (char ','))


-- parser for inventory phrases
inventoryP :: ReadP Command
inventoryP = whitespacify (
             pure (\_ -> Inventory)
             <*> string "inventory" <* eof )
             <++
             whitespacify (
             pure (\_ -> Inventory)
             <*> string "inventory " )


-- parser for take phrases
takeP :: ReadP Command
takeP = skipSpaces
        *> string "take" 
        <* munch1 isSpace
        *> pure (\i -> Take i)
        <*> nounPhrase


-- parser for use phrases
useP :: ReadP Command
useP = skipSpaces
        *> string "use" 
        <* munch1 isSpace
        *> pure (\i -> Use i)
        <*> nounPhrase


-- parser for exiting program
exitP :: ReadP Command
exitP = whitespacify $ pure (\_ -> Exit)
        <*> (string "exit" <|> string "quit")


-- parser look-ahead for checking string existence
haveString :: String -> ReadP ()
haveString str = do
        s <- look
        if elem str $ words s
        then pure ()
        else pfail


-- parser for dropping objects
dropP :: ReadP Command
dropP = skipSpaces
        *> string "drop"
        <* munch1 isSpace
        *> pure (\i -> Drop i)
        <*> nounPhrase


-- parser for look command
lookP :: ReadP Command
lookP = whitespacify (
        pure (\_ -> Look)
        <*> string "look" <* eof )
        <++
        whitespacify (
        pure (\_ -> Look)
        <*> string "look " )


-- parser for look-at items command
lookAtP :: ReadP Command
lookAtP = skipSpaces
            *> string "look" <* munch1 isSpace
            *> string "at" <* munch1 isSpace
            *> pure (\i -> LookAt i)
            <*> nounPhrase


-- parser for printing out command help
helpP :: ReadP Command
helpP = whitespacify (
        pure (\_ -> Help)
        <*> string "help" <* eof )
        <++
        whitespacify (
        pure (\_ -> Help)
        <*> string "help " )


-- parser for directions
directionP :: ReadP Direction
directionP = whitespacify (n <++ e <++ s <++ w)
    where
        n = pure (\_ -> N) <*> string "north"
        e = pure (\_ -> E) <*> string "east"
        s = pure (\_ -> S) <*> string "south"
        w = pure (\_ -> W) <*> string "west"


-- parser for move command
moveP :: ReadP Command
moveP = whitespacify (n <++ e <++ s <++ w)
    where
        n = pure (\_ -> Move N) <*> string "north"
        e = pure (\_ -> Move E) <*> string "east"
        s = pure (\_ -> Move S) <*> string "south"
        w = pure (\_ -> Move W) <*> string "west"


-- parser for commands
commandP :: ReadP Command
commandP = inventoryP
           <|> takeP
           <|> exitP
           <|> dropP
           <|> lookP
           <|> moveP
           <|> useP
           <|> lookAtP
           <|> helpP


-- parser for conjunctions, lists of commands
conjunctionP :: ReadP Conjunction
conjunctionP = pure (\i -> i)
               <*> commandP `sepBy` (string "and")
               <* eof


-- parser function of strings
parse :: String -> Maybe Conjunction
parse str = case runParser conjunctionP str of
        [(x, "")] -> Just x
        _         -> Nothing