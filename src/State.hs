
module State (State, createEmptyState, insertValue, readValue, state2Str) where

import qualified Data.Map as Map
import Data.List (intercalate)
import Stack

-- Definition of the State type
newtype State = State { getState :: Map.Map String StackElement }
  deriving (Show, Eq)

-- Creating an empty State
createEmptyState :: State
createEmptyState = State Map.empty

-- Inserting a value into the state and mapping it to a key
insertValue :: String -> StackElement -> State -> State
insertValue key value (State state) = State $ Map.insert key value state

-- Reading a value mapped from a key in the state
readValue :: String -> State -> StackElement
readValue key (State state) =
  case Map.lookup key state of
    Just value -> value
    Nothing    -> error "Run-time error"

-- Showing the element of the stack
showValue :: StackElement -> String
showValue (IntElement x) = show x
showValue (BoolElement x) = show x

-- Printing the state in the format: "key1=value1,key2=value2,..."
state2Str :: State -> String
state2Str (State state) = intercalate "," $ map (\(key, value) -> key ++ "=" ++ showValue value) (Map.toList state)
