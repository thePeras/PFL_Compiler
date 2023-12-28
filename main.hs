import Stack
import State
import Data.Char (isDigit, isAlpha)

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
data Aexp = Num Integer
          | Var String
          | AddExp Aexp Aexp
          | SubExp Aexp Aexp
          | MultExp Aexp Aexp
          deriving (Show)

-- Boolean expressions
data Bexp = TrueExp
          | FalseExp
          | EquExp Aexp Aexp -- Equality
          | EquBoolExp Bexp Bexp  -- Boolean equality
          | LeExp Aexp Aexp
          | AndExp Bexp Bexp
          | NotExp Bexp
          deriving (Show)

-- Statements
data Stm = Assign String Aexp
         | If Bexp Stm Stm
         | While Bexp Stm
         | Seq Stm Stm -- Sequence: stm1; stm2
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
compB (EquBoolExp x y) = compB x ++ compB y ++ [Equ]
compB (LeExp x y)      = compA x ++ compA y ++ [Le]
compB (AndExp x y)     = compB y ++ compB x ++ [And]
compB (NotExp x)       = compB x ++ [Neg] 

compile :: Program -> Code
compile [] = []
compile (stm:stms) = compileStm stm ++ compile stms

compileStm :: Stm -> Code
compileStm (Assign x a)      = compA a ++ [Store x]
compileStm (Seq stm1 stm2)   = compileStm stm1 ++ compileStm stm2
compileStm (While cond stm)     = [Loop (compB cond ++ compileStm stm) []]
compileStm (If cond stm1 stm2)  = compB cond ++ [Branch (compileStm stm1) (compileStm stm2)]

-- Tokenize
data Token = TInt Integer
           | TVar String
           | TPlus
           | TMinus
           | TMult
           | TLParen
           | TRParen
           | TTrue
           | TFalse
           | TEquBool
           | TEqu
           | TLe
           | TAnd
           | TNot
           | TAssign
           | TSeq
           | TIf
           | TThen
           | TElse
           | TWhile
           | TDo
           deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer input@(c:cs)
  | isDigit c = let (num, restNum) = span isDigit input
                in TInt (read num) : lexer restNum
  | isAlpha c = case keyword of
                  "and" -> TAnd : lexer restKeyword
                  "not" -> TNot : lexer restKeyword
                  "True" -> TTrue : lexer restKeyword
                  "False" -> TFalse : lexer restKeyword
                  "if" -> TIf : lexer restKeyword
                  "then" -> TThen : lexer restKeyword
                  "while" -> TWhile : lexer restKeyword
                  "do" -> TDo : lexer restKeyword
                  "else" -> TElse : lexer restKeyword
                  _     -> TVar var : lexer restVar
  | c == '+' = TPlus : lexer cs
  | c == '-' = TMinus : lexer cs
  | c == '*' = TMult : lexer cs
  | c == '(' = TLParen : lexer cs
  | c == ')' = TRParen : lexer cs
  | c == '=' && not (null cs) && c2 == '=' = TEqu : lexer newCs
  | c == '<' && not (null cs) && c2 == '=' = TLe : lexer newCs
  | c == '=' = TEquBool : lexer cs
  | c == ':' && not (null cs) && c2 == '=' = TAssign : lexer newCs
  | c == ';' = TSeq : lexer cs
  -- here can exist TEnd if we want know it is the last ;
  | otherwise = lexer cs -- Ignoring spaces, for example
  where (var, restVar) = span isAlpha input
        (keyword, restKeyword) = span isAlpha input
        (c2:newCs) = cs
        (followWord, restFollowWord) = span isAlpha newCs

parse :: String -> Program
parse program = parseStm (lexer program)

-- This predicate returns (Code, remaing)
getCode :: [Token] -> ([Token], [Token])
getCode (TLParen : t) = (takeWhile (/= TRParen) t, drop 1 $ dropWhile (/= TRParen) t)
getCode t = (takeWhile (/= TSeq) t, drop 1 $ dropWhile (/= TSeq) t)

parseStm :: [Token] -> Program
parseStm [] = []

parseStm (TIf : t) =
    case parseBexp cond of
        Just (parsedCond, []) ->
            let code1 = joinStms (parseStm t1)
                code2 = joinStms (parseStm t2)
            in If parsedCond code1 code2 : parseStm remaining
        _ -> error "Run-time error: Invalid if statement"
    where cond = takeWhile (/= TThen) t
          -- code1 is between TThen and TElse
          t111 = drop 1 $ dropWhile (/= TThen) t
          t11 = takeWhile (/= TElse) t111
          (t1, _) = getCode t11 -- if there is stm1, stm2 without () inside the then, stm2 will be ignored
          t22 = drop 1 $ dropWhile (/= TElse) t
          (t2, remaining) = getCode t22

parseStm (TWhile : t) =
    case parseBexp cond of
        Just (parsedCond, []) ->
            let code1 = joinStms (parseStm code)
            in While parsedCond code1 : parseStm remaining
        _ -> error "Run-time error: Invalid while statement1"
    where cond = takeWhile (/= TDo) t
          -- code between TDo and TSeq -- Not correct as the do can have more than one statement
          t1 = drop 1 $ dropWhile (/= TDo) t
          (code, remaining) = getCode t1

parseStm (TVar var : TAssign : t) =
    case parseAexp t of
        Just (parsedExpr, remaining) -> Assign var parsedExpr : parseStm remaining
        _ -> error "Parsing arithmetic expression failed"

parseStm (TSeq : t) = parseStm t

parseStm x = error $ "Run-time error: Invalid statement in parseStm: " ++ show x

joinStms :: [Stm] -> Stm
joinStms [] = error "Run-time error: Invalid statement in joinStms"
joinStms [stm] = stm
joinStms (stm:stms) = Seq stm (joinStms stms)

parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens = do
  (left, rest) <- parseAexpOp tokens
  parseAexpInReverse left rest

parseAexpInReverse :: Aexp -> [Token] -> Maybe (Aexp, [Token])
parseAexpInReverse left [] = return (left, [])
parseAexpInReverse left (TPlus : t) = do
  (right, remaining) <- parseAexpOp t
  parseAexpInReverse (AddExp left right) remaining
parseAexpInReverse left (TMinus : t) = do
  (right, remaining) <- parseAexpOp t
  parseAexpInReverse (SubExp left right) remaining
parseAexpInReverse left t = return (left, t)

parseAexpOp :: [Token] -> Maybe (Aexp, [Token])
parseAexpOp tokens = do
  (left, rest) <- parseAexpTerm tokens
  case rest of
    (TMult : t) -> do
      (right, remaining) <- parseAexpOp t
      return (MultExp left right, remaining)
    _ -> return (left, rest)

parseAexpTerm :: [Token] -> Maybe (Aexp, [Token])
parseAexpTerm (TLParen : tokens) =
  case parseAexp tokens of
    Just (exp, TRParen : restTokens) -> Just (exp, restTokens)
    _ -> Nothing
parseAexpTerm (TInt n : restTokens) = Just (Num n, restTokens)
parseAexpTerm (TVar x : restTokens) = Just (Var x, restTokens)
parseAexpTerm _ = Nothing

parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp tokens@(TLParen : _) = do
  (exp, rest) <- parseBexp (drop 1 $ tokens) -- Ignore the TLParen and parse the expression inside
  case rest of
    (TRParen : remaining) -> return (exp, remaining)
    _ -> Nothing -- Missing closing parenthesis
parseBexp tokens = parseRelationalBexp tokens

parseRelationalBexp :: [Token] -> Maybe (Bexp, [Token])
parseRelationalBexp tokens = do
  (left, rest) <- parseBasicBexp tokens
  parseRelationalBexpInReverse left rest

parseRelationalBexpInReverse :: Bexp -> [Token] -> Maybe (Bexp, [Token])
parseRelationalBexpInReverse left [] = return (left, [])
parseRelationalBexpInReverse left (TAnd : t) = do
  (right, remaining) <- parseBasicBexp t
  parseRelationalBexpInReverse (AndExp left right) remaining
parseRelationalBexpInReverse left (TEquBool : t) = do
  (right, remaining) <- parseBasicBexp t
  parseRelationalBexpInReverse (EquBoolExp left right) remaining
parseRelationalBexpInReverse left t = return (left, t)

parseBasicBexp :: [Token] -> Maybe (Bexp, [Token])
parseBasicBexp (TTrue : tokens) = Just (TrueExp, tokens)
parseBasicBexp (TFalse : tokens) = Just (FalseExp, tokens)
parseBasicBexp (TNot : tokens) = do
  (exp, rest) <- parseBexp tokens
  return (NotExp exp, rest)
parseBasicBexp tokens = parseRelationalAexp tokens

parseRelationalAexp :: [Token] -> Maybe (Bexp, [Token])
parseRelationalAexp tokens@(TLParen : _) = parseBexp tokens -- If the expression starts with a parenthesis, parse it directly
parseRelationalAexp tokens = do
  (left, rest) <- parseAexp tokens
  case rest of
    (TEqu : t) -> do
      (right, remaining) <- parseAexp t
      return (EquExp left right, remaining)
    (TLe : t) -> do
      (right, remaining) <- parseAexp t
      return (LeExp left right, remaining)
    _ -> Nothing -- Invalid relational expression

-- (problems with parenthesis)
--    ex: parseBexp (lexer "(2<=3) and (2 == 4)")
--    ex: parseBexp (lexer "(2 <= 5 = 3 == 4) and (2 <= 2)")

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