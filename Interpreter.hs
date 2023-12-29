module Interpreter (interprete, Inst(..), Code) where

import Stack
import State

-- Definition of the Inst type
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

-- Definition of the Code type
type Code = [Inst]

interprete :: Inst -> (Code, Stack, State) -> (Code, Stack, State)

-- Push
interprete (Push x) (code, stack, state) = 
  (code, IntElement x : stack, state)

-- Add
interprete Add (code, IntElement x : IntElement y : xs, state) = 
  (code, IntElement (x+y) : xs, state)
interprete Add (code, stack, state) = error $ "Run-time error"

-- Mult
interprete Mult (code, IntElement x : IntElement y : xs, state) = 
  (code, IntElement (x*y) : xs, state)
interprete Mult (code, stack, state) = error $ "Run-time error"

-- Sub
interprete Sub (code, IntElement x : IntElement y : xs, state) = 
  (code, IntElement (x-y) : xs, state)
interprete Sub (code, stack, state) = error $ "Run-time error"

-- Tru
interprete Tru (code, stack, state) = 
  (code, BoolElement True : stack, state)

-- Fals
interprete Fals (code, stack, state) = 
  (code, BoolElement False : stack, state)

-- Equ
interprete Equ (code, IntElement x : IntElement y : xs, state) = 
  (code, BoolElement (x == y) : xs, state)
interprete Equ (code, BoolElement x : BoolElement y : xs, state) = 
  (code, BoolElement (x == y) : xs, state)
interprete Equ (code, stack, state) = error $ "Run-time error"

-- Le
interprete Le (code, IntElement x : IntElement y : xs, state) = 
  (code, BoolElement (x <= y) : xs, state)
interprete Le (code, stack, state) 
  = error $ "Run-time error"

-- And
interprete And (code, BoolElement x : BoolElement y : xs, state) = 
  (code, BoolElement (x && y) : xs, state)
interprete And (code, stack, state) = error $ "Run-time error"

-- Neg
interprete Neg (code, BoolElement x : xs, state) = 
  (code, BoolElement (not x) : xs, state)
interprete Neg (code, stack, state) = error $ "Run-time error"

-- Fetch
interprete (Fetch key) (code, stack, state)
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
interprete (Store key) (code, value : stack, state) = 
  (code, stack, insertValue key value state)

-- Noop
interprete Noop (code, stack, state) = 
  (code, stack, state)

-- Branch
interprete (Branch code1 code2) (code, BoolElement x : xs, state)
  | x = (code1 ++ code, xs, state)
  | otherwise = (code2 ++ code, xs, state)
interprete (Branch code1 code2) (code, stack, state) = error $ "Run-time error"

-- Loop
interprete (Loop cond code1) (code, stack, state) =
  (cond ++ [Branch (code1 ++ [Loop cond code1]) [Noop]], stack, state)