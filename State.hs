
module State (State, createEmptyState) where

import qualified Data.Map as Map
import Data.List (intercalate)

data StateType = IntType Integer | BoolType Bool
  deriving (Show, Eq)

newtype State = State { getState :: Map.Map String StateType }
  deriving (Show, Eq)

createEmptyState :: State
createEmptyState = State Map.empty

insertValue :: String -> StateType -> State -> State
insertValue key value (State state) = State $ Map.insert key value state

readValue :: String -> State -> StateType
readValue key (State state) =
  case Map.lookup key state of
    Just value -> value
    Nothing    -> error "No value found for the given key"

showVal :: StateType -> String
showVal (IntType x) = show x
showVal (BoolType x) = show x

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