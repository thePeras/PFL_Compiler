
module State (State, createEmptyState, insertValue, readValue, state2Str) where

import qualified Data.Map as Map
import Data.List (intercalate)
import Stack

-- Definition of the State type
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

showValue :: StackElement -> String
showValue (IntElement x) = show x
showValue (BoolElement x) = show x

state2Str :: State -> String
state2Str (State state) = intercalate "," $ map (\(key, value) -> key ++ "=" ++ showValue value) (Map.toList state)

-- Documentation:

-- Creating an empty State
-- myState = emptyState

-- Inserting values
-- updatedState = insertValue "key1" 3.14 myState -- This will return a new State with the value inserted

-- Reading values
-- valueForKey1 = readValue "key1" updatedState  -- This will return the "key1" value or throw error is "key1" doesn't exist-}