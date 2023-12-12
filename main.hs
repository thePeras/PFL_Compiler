import Stack

import State

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Code = [Inst]

ff = 0
tt = 1

operation :: (Integer -> Integer -> Integer) -> Stack -> Stack
operation op stack =
  let x = top stack
      y = top (pop stack)
      stackWithoutXY = pop (pop stack)
      result = x `op` y
      newStack = push result stackWithoutXY
  in newStack

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Add : xs), stack, state) = run (xs, operation (+) stack, state)
run ((Mult : xs), stack, state) = run (xs, operation (*) stack, state)
run ((Sub : xs), stack, state) = run (xs, operation (-) stack, state)

-- TODO: Check this (maybe remove operation)
run ((Equ : xs), stack, state) = run (xs, operation (\x y -> if x == y then 1 else 0) stack, state)
run ((Le : xs), stack, state) = run (xs, operation (\x y -> if x <= y then 1 else 0) stack, state)

run ((Push value : xs), stack, state) = run (xs, push value stack, state)
run ((Tru : xs), stack, state) = run (xs, push tt stack, state)
run ((Fals : xs), stack, state) = run (xs, push ff stack, state)
run ((Fetch key : xs), stack, state) = run (xs, push (readValue key state) stack, state)
run ((Store key : xs), stack, state) = run (xs, pop stack, insertValue key (top stack) state)

run ((Branch code1 code2 : xs), stack, state) =
    case top stack of
        0 -> run (code2 ++ xs, pop stack, state)
        _ -> run (code1 ++ xs, pop stack, state)

run ((Noop : xs), stack, state) = run (xs, stack, state)
run ((Loop code1 code2 : xs), stack, state) =
    case top stack of
        0 -> run (xs, pop stack, state)
        _ -> run (code1 ++ [Loop code1 code2] ++ code2 ++ xs, pop stack, state)

-- TODO: Check this (maybe remove operation)
run ((And : xs), stack, state) = run (xs, operation (\x y -> if x == 1 && y == 1 then 1 else 0) stack, state)
run ((Neg : xs), stack, state) = run (xs, operation (\x y -> if x == 1 then 0 else 1) stack, state)


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")