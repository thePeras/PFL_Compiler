import Stack
import State

-- Part 1

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Code = [Inst]

operator :: Inst -> (Code, Stack, State) -> (Code, Stack, State)

-- Push
operator (Push x) (code, stack, state) = 
  (code, IntElement x : stack, state)

-- Add
operator Add (code, IntElement x : IntElement y : xs, state) = 
  (code, IntElement (x+y) : xs, state)
operator Add (code, stack, state) = error $ "Run-time error"

-- Mult
operator Mult (code, IntElement x : IntElement y : xs, state) = 
  (code, IntElement (x*y) : xs, state)
operator Mult (code, stack, state) = error $ "Run-time error"

-- Sub
operator Sub (code, IntElement x : IntElement y : xs, state) = 
  (code, IntElement (x-y) : xs, state)
operator Sub (code, stack, state) = error $ "Run-time error"

-- Tru
operator Tru (code, stack, state) = 
  (code, BoolElement True : stack, state)

-- Fals
operator Fals (code, stack, state) = 
  (code, BoolElement False : stack, state)

-- Equ
operator Equ (code, IntElement x : IntElement y : xs, state) = 
  (code, BoolElement (x == y) : xs, state)
operator Equ (code, BoolElement x : BoolElement y : xs, state) = 
  (code, BoolElement (x == y) : xs, state)
operator Equ (code, stack, state) = error $ "Run-time error"

-- Le
operator Le (code, IntElement x : IntElement y : xs, state) = 
  (code, BoolElement (x <= y) : xs, state)
operator Le (code, stack, state) 
  = error $ "Run-time error"

-- And
operator And (code, BoolElement x : BoolElement y : xs, state) = 
  (code, BoolElement (x && y) : xs, state)
operator And (code, stack, state) = error $ "Run-time error"

-- Neg
operator Neg (code, BoolElement x : xs, state) = 
  (code, BoolElement (not x) : xs, state)
operator Neg (code, stack, state) = error $ "Run-time error"

-- Fetch
operator (Fetch key) (code, stack, state)
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
operator (Store key) (code, value : stack, state) = 
  (code, stack, insertValue key value state)

-- Noop
operator Noop (code, stack, state) = 
  (code, stack, state)

-- Branch
operator (Branch code1 code2) (code, BoolElement x : xs, state)
  | x = (code1 ++ code, xs, state)
  | otherwise = (code2 ++ code, xs, state)
operator (Branch code1 code2) (code, stack, state) = error $ "Run-time error"

-- Loop
operator (Loop c1 c2) (code, stack, state) =
  (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]], stack, state)

  
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (x:xs, stack, state) = run (newCode, newStack, newState)
  where (newCode, newStack, newState) = operator x (xs, stack, state)


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

--runTests :: Bool
--runTests = 
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
data Aexp = Num Integer                -- Constants
          | Var String             -- Variables
          | AddExp Aexp Aexp
          | SubExp Aexp Aexp
          | MultExp Aexp Aexp
          deriving (Show)

-- Boolean expressions
data Bexp = TrueExp                   -- True constant
          | FalseExp                  -- False constant
          | EquExp Aexp Aexp            -- Equality
          | LeExp Aexp Aexp            -- Less or equal
          | AndExp Bexp Bexp           -- Logical AND
          | NotExp Bexp                -- Logical NOT
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
compA (AddExp x y)   = compA y ++ compA x ++ [Add]
compA (SubExp x y)   = compA y ++ compA x ++ [Sub]
compA (MultExp x y)   = compA y ++ compA x ++ [Mult]

compB :: Bexp -> Code
compB TrueExp         = [Tru]
compB FalseExp        = [Fals]
compB (EquExp x y)      = compA x ++ compA y ++ [Equ]
compB (LeExp x y)      = compA x ++ compA y ++ [Le]
compB (AndExp x y)     = compB y ++ compB x ++ [And] -- This can be wrong
compB (NotExp x)       = compB x ++ [Neg]

compile :: Program -> Code
compile [] = []
compile (stm:stms) = compileStm stm ++ compile stms

compileStm :: Stm -> Code
compileStm (Assign x a)      = compA a ++ [Store x]
compileStm (Seq stm1 stm2)   = compileStm stm1 ++ compileStm stm2
compileStm (While b stm)     = [Loop (compB b ++ compileStm stm) []]
compileStm (If b stm1 stm2)  = compB b ++ [Branch (compileStm stm1) (compileStm stm2)]

parse :: String -> Program
parse programCode = []
--parse programCode = parseStm $ splitBy ';' programCode

--parseStm :: [String] -> Program
--parseStm [] = error $ "Run-time error"
--parseStm [stm] = [parseStm' (words stm)]
--parseStm (stm1:stms) = parseStm' (words stm1) ++ parseStm stms

--parseStm' :: [String] -> Stm
--parseStm' [] = error $ "Run-time error"
--parseStm' (x:":=":xs) = Assign x (parseAexp' xs)
--parseStm' ("while":xs) = While cond code
--  where cond = parseBexp xs
--        code = parseStm' stm
--        stm = dropWhile (/= "do") xs
-- while (not(i == 1)) do (fact := fact * i; i := i - 1;)
-- [while, (not(i, ==, 1)), do, (fact, :=, fact, *, i;, i, :=, i, -, 1;)])]    
--parseStm' ('if':xs) = If (parseBexp xs) (parseStm' stm1) (parseStm' stm2)

--parseAexp :: String -> Aexp
--parseAexp str = parseAexp' (words str)

--parseAexp' :: [String] -> Aexp
--parseAexp' [] = error $ "Run-time error"
--parseAexp' [x] = Var x
--parseAexp' (x:"+":xs) = AddExp (parseAexp x) (parseAexp' xs)
--parseAexp' (x:"-":xs) = SubExp (parseAexp x) (parseAexp' xs)
--parseAexp' (x:"*":xs) = MultExp (parseAexp x) (parseAexp' xs)

--parseBexp :: String -> Bexp
--parseBexp str = parseBexp' (words str)

--parseBexp' :: [String] -> Bexp
--parseBexp' [] = error $ "Run-time error"
--parseBexp' [x] = Var x
--parseBexp' ("not":xs) = NotExp (parseBexp' xs)
--parseBexp' (x:"and":xs) = AndExp (parseBexp x) (parseBexp' xs)
--parseBexp' (x:"<=":xs) = LeExp (parseAexp x) (parseAexp' xs)
--parseBexp' (x:"==":xs) = EquExp (parseAexp x) (parseAexp' xs)

--splitBy :: Char -> String -> [String]
--splitBy _ [] = []
--splitBy c str = first : splitBy c rest
--  where first = takeWhile (/= c) str
--        rest = drop (length first + 1) str

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")