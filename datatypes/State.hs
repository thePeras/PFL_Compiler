module State (createEmptyState) where

import Stack
import Code
import Storage

type State = (Code, Stack, Storage)

createEmptyState :: State
createEmptyState = ([], empty, emptyStorage)

state2Str :: State -> String
state2Str (code, stack, storage) = "Code: " ++ code2Str code ++ "\nStack: " ++ stack2Str stack ++ "\nStorage: " ++ storage2Str storage 