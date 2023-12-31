module Parser where

import Compiler(Aexp(..), Bexp(..), Stm(..), Program)
import Lexer

-- Main function to parse a program
parse :: String -> Program
parse program = parseStm (lexer program)

-- Helper function returning a tuple (ExtractedCode, RemaingCode)
getCode :: [Token] -> ([Token], [Token])
getCode (TLParen : t) = do
    let (extractedCode, remaining) = getCodeToRParen t [] [TLParen]
    (extractedCode, remaining)
getCode t = (takeWhile (/= TSeq) t, drop 1 $ dropWhile (/= TSeq) t)

-- Recursively extract code until the matching right parenthesis is found
getCodeToRParen :: [Token] -> [Token] -> [Token] -> ([Token], [Token])
getCodeToRParen [] code stack = error "Run-time error"
getCodeToRParen (TLParen : remaining) code stack = getCodeToRParen remaining (code ++ [TLParen]) (TLParen:stack)
getCodeToRParen (TRParen : remaining) code (TLParen:[]) = (code, remaining)
getCodeToRParen (TRParen : remaining) code (TLParen:stack) = getCodeToRParen remaining (code ++ [TRParen]) stack
getCodeToRParen (x : remaining) code stack = getCodeToRParen remaining (code ++ [x]) stack

-- Recursively parse statements
parseStm :: [Token] -> Program
parseStm [] = []

parseStm (TIf : t) =
    case parseBexp cond of
        Just (parsedCond, []) ->
            let code1 = joinStms (parseStm extractedCode1)
                code2 = joinStms (parseStm extractedCode2)
            in If parsedCond code1 code2 : parseStm remaining
        _ -> error "Run-time error"
    where cond = takeWhile (/= TThen) t
          -- [(...) TThen (code1) TElse (code2)]
          afterThen = drop 1 $ dropWhile (/= TThen) t
          beforeElse = takeWhile (/= TElse) afterThen
          (extractedCode1, _) = getCode beforeElse -- note: (...) then stm1; stm2; else (...) Without (), stm2 will be ignored
          afterElse = drop 1 $ dropWhile (/= TElse) t
          (extractedCode2, remaining) = getCode afterElse

parseStm (TWhile : t) =
    case parseBexp cond of
        Just (parsedCond, []) ->
            let code = joinStms (parseStm extractedCode)
            in While parsedCond code : parseStm remaining
        _ -> error "Run-time error"
    where cond = takeWhile (/= TDo) t
          -- [(...) TDo (code)]
          afterDo = drop 1 $ dropWhile (/= TDo) t
          (extractedCode, remaining) = getCode afterDo

parseStm (TVar var : TAssign : t) =
    case parseAexp t of
        Just (parsedExpr, remaining) -> Assign var parsedExpr : parseStm remaining
        _ -> error "Parsing arithmetic expression failed"

parseStm (TSeq : t) = parseStm t

parseStm x = error $ "Run-time error"

-- Helper function to join a list of statements into a single statement
joinStms :: [Stm] -> Stm
joinStms [] = error "Run-time error"
joinStms [stm] = stm
joinStms (stm:stms) = Seq stm (joinStms stms)

-- Helper function to parse arithmetic expressions
parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens = do
  (left, rest) <- parseAexpOp tokens
  parseAexpInReverse left rest

-- Helper function to parse arithmetic expressions in reverse order
parseAexpInReverse :: Aexp -> [Token] -> Maybe (Aexp, [Token])
parseAexpInReverse left [] = return (left, [])
parseAexpInReverse left (TPlus : t) = do
  (right, remaining) <- parseAexpOp t
  parseAexpInReverse (AddExp left right) remaining
parseAexpInReverse left (TMinus : t) = do
  (right, remaining) <- parseAexpOp t
  parseAexpInReverse (SubExp left right) remaining
parseAexpInReverse left t = return (left, t)

-- Helper function to parse arithmetic expressions with operators
parseAexpOp :: [Token] -> Maybe (Aexp, [Token])
parseAexpOp tokens = do
  (left, rest) <- parseAexpTerm tokens
  case rest of
    (TMult : t) -> do
      (right, remaining) <- parseAexpOp t
      return (MultExp left right, remaining)
    _ -> return (left, rest)

-- Helper function to parse arithmetic expressions without operators
parseAexpTerm :: [Token] -> Maybe (Aexp, [Token])
parseAexpTerm (TLParen : tokens) =
  case parseAexp tokens of
    Just (exp, TRParen : restTokens) -> Just (exp, restTokens)
    _ -> Nothing
parseAexpTerm (TInt n : restTokens) = Just (Num n, restTokens)
parseAexpTerm (TVar x : restTokens) = Just (Var x, restTokens)
parseAexpTerm _ = Nothing

-- Helper function to parse boolean expressions
parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp tokens = parseAndExp tokens

-- Helper function to parse boolean expressions with AND operator
parseAndExp :: [Token] -> Maybe (Bexp, [Token])
parseAndExp tokens = do
  (left, rest) <- parseEquBoolExp tokens
  parseAndExpInReverse left rest

-- Helper function to parse boolean expressions with AND operator in reverse order
parseAndExpInReverse :: Bexp -> [Token] -> Maybe (Bexp, [Token])
parseAndExpInReverse left [] = return (left, [])
parseAndExpInReverse left (TAnd : t) = do
  (right, remaining) <- parseEquBoolExp t
  parseAndExpInReverse (AndExp left right) remaining
parseAndExpInReverse left t = return (left, t)

-- Helper function to parse boolean expressions with EQUALITY operator
parseEquBoolExp :: [Token] -> Maybe (Bexp, [Token])
parseEquBoolExp tokens = do
  (left, rest) <- parseBasicBexp tokens
  parseEquBoolExpInReverse left rest

-- Helper function to parse boolean expressions with EQUALITY operator in reverse order
parseEquBoolExpInReverse :: Bexp -> [Token] -> Maybe (Bexp, [Token])
parseEquBoolExpInReverse left [] = return (left, [])
parseEquBoolExpInReverse left (TEquBool : t) = do
  (right, remaining) <- parseBasicBexp t
  parseEquBoolExpInReverse (EquBoolExp left right) remaining
parseEquBoolExpInReverse left t = return (left, t)

-- Helper function to parse basic boolean expressions
parseBasicBexp :: [Token] -> Maybe (Bexp, [Token])
parseBasicBexp (TTrue : tokens) = Just (TrueExp, tokens)
parseBasicBexp (TFalse : tokens) = Just (FalseExp, tokens)
parseBasicBexp (TNot : tokens) = do
  (exp, rest) <- parseBasicBexp tokens
  return (NotExp exp, rest)
parseBasicBexp (TLParen : tokens) = 
  case parseBexp tokens of
    Just (exp, TRParen : restTokens) -> Just (exp, restTokens)
    _ -> Nothing
parseBasicBexp tokens = parseRelationalAexp tokens

-- Helper function to parse relational expressions
parseRelationalAexp :: [Token] -> Maybe (Bexp, [Token])
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