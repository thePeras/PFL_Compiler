
module State (State, createEmptyState, insertValue, readValue, state2Str) where

import qualified Data.Map as Map

newtype State = State { getState :: Map.Map String Integer }

createEmptyState :: State
createEmptyState = State Map.empty

insertValue :: String -> Integer -> State -> State
insertValue key value (State state) = State $ Map.insert key value state

readValue :: String -> State -> Integer
readValue key (State state) =
  case Map.lookup key state of
    Just value -> value
    Nothing    -> error "No value found for the given key"

state2Str :: State -> String
state2Str (State state) = Map.foldrWithKey (\key value acc -> key ++ "=" ++ show value ++ "," ++ acc) "" state


-- Documentation:

-- Creating an empty State
-- myState = emptyState

-- Inserting values
-- updatedState = insertValue "key1" 3.14 myState -- This will return a new State with the value inserted

-- Reading values
-- valueForKey1 = readValue "key1" updatedState  -- This will return the "key1" value or throw error is "key1" doesn't exist