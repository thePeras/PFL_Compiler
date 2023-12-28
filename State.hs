
module State (State, createEmptyState, insertValue, readValue, showVal, state2Str) where

import qualified Data.Map as Map
import Data.List (intercalate)
import Stack

newtype State = State { getState :: Map.Map String StackElement }
  deriving (Show, Eq)

createEmptyState :: State
createEmptyState = State Map.empty

insertValue :: String -> StackElement -> State -> State
insertValue key value (State state) = State $ Map.insert key value state

readValue :: String -> State -> StackElement
readValue key (State state) =
  case Map.lookup key state of
    Just value -> value
    Nothing    -> error "Run-time error"

showVal :: StackElement -> String
showVal (IntElement x) = show x
showVal (BoolElement x) = show x

-- TODO: Make a custom
state2Str :: State -> String
state2Str (State state) = intercalate "," $ map (\(key, value) -> key ++ "=" ++ showVal value) (Map.toList state)



-- Documentation:

-- Creating an empty State
-- myState = emptyState

-- Inserting values
-- updatedState = insertValue "key1" 3.14 myState -- This will return a new State with the value inserted

-- Reading values
-- valueForKey1 = readValue "key1" updatedState  -- This will return the "key1" value or throw error is "key1" doesn't exist-}