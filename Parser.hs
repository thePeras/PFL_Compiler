module Parser where

import Compiler(Aexp(..), Bexp(..), Stm(..), Program)
import Lexer

parse :: String -> Program
parse program = parseStm (lexer program)

-- Helper predicate returning a tuple (ExtractedCode, RemaingCode)
getCode :: [Token] -> ([Token], [Token])
getCode (TLParen : t) = (takeWhile (/= TRParen) t, drop 1 $ dropWhile (/= TRParen) t)
getCode t = (takeWhile (/= TSeq) t, drop 1 $ dropWhile (/= TSeq) t)

parseStm :: [Token] -> Program
parseStm [] = []

parseStm (TIf : t) =
    case parseBexp cond of
        Just (parsedCond, []) ->
            let code1 = joinStms (parseStm extractedCode1)
                code2 = joinStms (parseStm extractedCode2)
            in If parsedCond code1 code2 : parseStm remaining
        _ -> error "Run-time error: Invalid if statement"
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
        _ -> error "Run-time error: Invalid while statement1"
    where cond = takeWhile (/= TDo) t
          -- [(...) TDo (code)]
          afterDo = drop 1 $ dropWhile (/= TDo) t
          (extractedCode, remaining) = getCode afterDo

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
parseBexp tokens = parseAndExp tokens

parseAndExp :: [Token] -> Maybe (Bexp, [Token])
parseAndExp tokens = do
  (left, rest) <- parseEquBoolExp tokens
  parseAndExpInReverse left rest

parseAndExpInReverse :: Bexp -> [Token] -> Maybe (Bexp, [Token])
parseAndExpInReverse left [] = return (left, [])
parseAndExpInReverse left (TAnd : t) = do
  (right, remaining) <- parseEquBoolExp t
  parseAndExpInReverse (AndExp left right) remaining
parseAndExpInReverse left t = return (left, t)

parseEquBoolExp :: [Token] -> Maybe (Bexp, [Token])
parseEquBoolExp tokens = do
  (left, rest) <- parseBasicBexp tokens
  parseEquBoolExpInReverse left rest

parseEquBoolExpInReverse :: Bexp -> [Token] -> Maybe (Bexp, [Token])
parseEquBoolExpInReverse left [] = return (left, [])
parseEquBoolExpInReverse left (TEquBool : t) = do
  (right, remaining) <- parseBasicBexp t
  parseEquBoolExpInReverse (EquBoolExp left right) remaining
parseEquBoolExpInReverse left t = return (left, t)

parseBasicBexp :: [Token] -> Maybe (Bexp, [Token])
parseBasicBexp (TTrue : tokens) = Just (TrueExp, tokens)
parseBasicBexp (TFalse : tokens) = Just (FalseExp, tokens)
parseBasicBexp (TNot : tokens) = do
  (exp, rest) <- parseBasicBexp tokens
  return (NotExp exp, rest)
parseBasicBexp (TLParen : tokens) = do
  (exp, rest) <- parseAndExp tokens
  case rest of
    (TRParen : remaining) -> return (exp, remaining)
    _ -> Nothing -- Missing closing parenthesis
parseBasicBexp tokens = parseRelationalAexp tokens

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