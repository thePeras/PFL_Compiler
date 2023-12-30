module Interpreter where

import Stack
import State

-- Definition of the Inst type
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

-- Definition of the Code type
type Code = [Inst]

interpret :: Inst -> (Code, Stack, State) -> (Code, Stack, State)

-- Push
interpret (Push x) (code, stack, state) = 
  (code, IntElement x : stack, state)

-- Add
interpret Add (code, IntElement x : IntElement y : xs, state) = 
  (code, IntElement (x+y) : xs, state)
interpret Add (code, stack, state) = error $ "Run-time error"

-- Mult
interpret Mult (code, IntElement x : IntElement y : xs, state) = 
  (code, IntElement (x*y) : xs, state)
interpret Mult (code, stack, state) = error $ "Run-time error"

-- Sub
interpret Sub (code, IntElement x : IntElement y : xs, state) = 
  (code, IntElement (x-y) : xs, state)
interpret Sub (code, stack, state) = error $ "Run-time error"

-- Tru
interpret Tru (code, stack, state) = 
  (code, BoolElement True : stack, state)

-- Fals
interpret Fals (code, stack, state) = 
  (code, BoolElement False : stack, state)

-- Equ
interpret Equ (code, IntElement x : IntElement y : xs, state) = 
  (code, BoolElement (x == y) : xs, state)
interpret Equ (code, BoolElement x : BoolElement y : xs, state) = 
  (code, BoolElement (x == y) : xs, state)
interpret Equ (code, stack, state) = error $ "Run-time error"

-- Le
interpret Le (code, IntElement x : IntElement y : xs, state) = 
  (code, BoolElement (x <= y) : xs, state)
interpret Le (code, stack, state) 
  = error $ "Run-time error"

-- And
interpret And (code, BoolElement x : BoolElement y : xs, state) = 
  (code, BoolElement (x && y) : xs, state)
interpret And (code, stack, state) = error $ "Run-time error"

-- Neg
interpret Neg (code, BoolElement x : xs, state) = 
  (code, BoolElement (not x) : xs, state)
interpret Neg (code, stack, state) = error $ "Run-time error"

-- Fetch
interpret (Fetch key) (code, stack, state)
  | valueIsInt = (code, value : stack, state)
  | valueIsBool = (code, value : stack, state)
  | otherwise = error "Run-time error"
  where
    value = readValue key state
    valueIsInt = case value of
      IntElement _ -> True
      _ -> False
    valueIsBool = case value of
      BoolElement _ -> True
      _ -> False

-- Store
interpret (Store key) (code, value : stack, state) = 
  (code, stack, insertValue key value state)

-- Noop
interpret Noop (code, stack, state) = 
  (code, stack, state)

-- Branch
interpret (Branch code1 code2) (code, BoolElement x : xs, state)
  | x = (code1 ++ code, xs, state)
  | otherwise = (code2 ++ code, xs, state)
interpret (Branch code1 code2) (code, stack, state) = error $ "Run-time error"

-- Loop
interpret (Loop cond code1) (code, stack, state) =
  (cond ++ [Branch (code1 ++ [Loop cond code1]) [Noop]], stack, state)