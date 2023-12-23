import Stack
import State

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Code = [Inst]

-- True and False representations
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

-- TODO: Chamo a atenção que na primeira alínea do trabalho de Haskell a função run 
-- quando chamada com configurações erradas deve retornar a mensagem de erro: "Run-time error", 
-- chamando a função error $ "Run-time error".
-- Exemplo:
-- run([Push 1,Push 2,And], createEmptyStack, createEmptyState)
-- deve imprimir a mensagem de erro "Run-time error".

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

-- TODO: this is wrong, ig
run ((Loop code1 code2 : xs), stack, state) =
    case top stack of
        0 -> run (xs, pop stack, state)
        _ -> run (code1 ++ [Loop code1 code2] ++ code2 ++ xs, pop stack, state)

-- TODO: This is wrong
run ((And : xs), stack, state) = run (xs, operation (\x y -> if x == 1 && y == 1 then 1 else 0) stack, state)
run ((Neg : xs), stack, state) = run (xs, operation (\x y -> if x == 1 then 0 else 1) stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

runTests :: Bool
runTests = 
  -- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
  -- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
  -- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
  -- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
  -- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
  -- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
  -- testAssembler [Push (-20),Push (-21), Le] == ("True","")
  -- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
  -- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
  -- If you test:
  -- testAssembler [Push 1,Push 2,And]
  -- You should get an exception with the string: "Run-time error"
  -- If you test:
  -- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
  -- You should get an exception with the string: "Run-time error"


-- Part 2

-- Arithmetic expressions
data Aexp = Num Int                -- Constants
          | Var String             -- Variables
          | Add Aexp Aexp
          | Sub Aexp Aexp
          | Mul Aexp Aexp
          deriving (Show)

-- Boolean expressions
data Bexp = TrueB                   -- True constant
          | FalseB                  -- False constant
          | Eq Aexp Aexp            -- Equality
          | Not Bexp                -- Logical NOT
          deriving (Show)

-- Statements
data Stm = Assign String Aexp      -- Assignment: x := a
         | Seq Stm Stm             -- Sequence: stm1; stm2
         | If Bexp Stm Stm         -- If-Else statement
         | While Bexp Stm          -- While loop
         deriving (Show)

-- Program
type Program = [Stm]

-- Ex: x + 1 is [push−1, fetch−x, add]
compA :: Aexp -> Code
compA (Num n)     = [Push n]
compA (Var x)     = [Fetch x]
compA (Add x y)   = compA y ++ compA x ++ [Add]
compA (Sub x y)   = compA y ++ compA x ++ [Sub]
compA (Mul x y)   = compA y ++ compA x ++ [Mul]

compB :: Bexp -> Code
compB TrueB         = [Tru]
compB FalseB        = [Fals]
compB (Eq x y)      = compA x ++ compA y ++ [Eq]
compB (Le x y)      = compA x ++ compA y ++ [Le]
compB (And x y)     = compB y ++ compB x ++ [And]
compB (Not x)       = compB x ++ [Neg]

compile :: Stm -> Code
compile (Assign x a)      = compA a ++ [Store x]
compile (Seq stm1 stm2)   = compile stm1 ++ compile stm2
compile (If b stm1 stm2)  = compB b ++ [Branch (compile stm1) (compile stm2)]
compile (While b stm)     = [Loop (compB b ++ compile stm)]
compile _                 = error $ "Run-time error"

parse :: String -> Program
parse = map parseStm statements
  where statements = words . filter (/= ';')

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, store2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")