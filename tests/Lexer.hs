import Lexer

main :: IO ()
main = do
    putStrLn $ "Test while token: " ++ show (testLexer "while" == [TWhile])
    putStrLn $ "Test if token: " ++ show (testLexer "if" == [TIf])
    putStrLn $ "Test then token: " ++ show (testLexer "then" == [TThen])
    putStrLn $ "Test else token: " ++ show (testLexer "else" == [TElse])
    putStrLn $ "Test not token: " ++ show (testLexer "not" == [TNot])
    putStrLn $ "Test plus token: " ++ show (testLexer "+" == [TPlus])
    putStrLn $ "Test minus token: " ++ show (testLexer "-" == [TMinus])
    putStrLn $ "Test times token: " ++ show (testLexer "*" == [TMult])
    putStrLn $ "Test semicolon token: " ++ show (testLexer ";" == [TSeq])
    putStrLn $ "Test lbracket token: " ++ show (testLexer "(" == [TLParen])
    putStrLn $ "Test rbracket token: " ++ show (testLexer ")" == [TRParen])
    putStrLn $ "Test assign token: " ++ show (testLexer ":=" == [TAssign])
    putStrLn $ "Test eq token: " ++ show (testLexer "=" == [TEquBool])
    putStrLn $ "Test eq token: " ++ show (testLexer "==" == [TEqu])
    putStrLn $ "Test and token: " ++ show (testLexer "and" == [TAnd])
    putStrLn $ "Test lesseq token: " ++ show (testLexer "<=" == [TLe])
    putStrLn $ "Test integer token: " ++ show (testLexer "42" == [TInt 42])
    putStrLn $ "Test bool token: " ++ show (testLexer "True" == [TTrue])
    putStrLn $ "Test bool token: " ++ show (testLexer "False" == [TFalse])
    putStrLn $ "Test var token: " ++ show (testLexer "x" == [TVar "x"])

    putStrLn $ "Test 1: " ++ show (lexer "x := 5; x := x - 1;" == [TVar "x", TAssign, TInt 5, TSeq, TVar "x", TAssign, TVar "x", TMinus, TInt 1, TSeq])
    putStrLn $ "Test 2: " ++ show (lexer "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == [TIf, TLParen, TNot, TTrue, TAnd, TInt 2, TLe, TInt 5, TEquBool, TInt 3, TEqu, TInt 4, TRParen, TThen, TVar "x", TAssign, TInt 1, TSeq, TElse, TVar "y", TAssign, TInt 2, TSeq])
    putStrLn $ "Test 3: " ++ show (lexer "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == [TVar "x", TAssign, TInt 42, TSeq, TIf, TVar "x", TLe, TInt 43, TThen, TVar "x", TAssign, TInt 1, TSeq, TElse, TLParen, TVar "x", TAssign, TInt 33, TSeq, TVar "x", TAssign, TVar "x", TPlus, TInt 1, TSeq, TRParen])
    putStrLn $ "Test 4: " ++ show (lexer "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == [TVar "x", TAssign, TInt 42, TSeq, TIf, TVar "x", TLe, TInt 43, TThen, TVar "x", TAssign, TInt 1, TSeq, TElse, TVar "x", TAssign, TInt 33, TSeq, TVar "x", TAssign, TVar "x", TPlus, TInt 1, TSeq])
    putStrLn $ "Test 5: " ++ show (lexer "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == [TVar "x", TAssign, TInt 42, TSeq, TIf, TVar "x", TLe, TInt 43, TThen, TVar "x", TAssign, TInt 1, TSeq, TElse, TVar "x", TAssign, TInt 33, TSeq, TVar "x", TAssign, TVar "x", TPlus, TInt 1, TSeq, TVar "z", TAssign, TVar "x", TPlus, TVar "x", TSeq])
    putStrLn $ "Test 6: " ++ show (lexer "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == [TVar "x", TAssign, TInt 2, TSeq, TVar "y", TAssign, TLParen, TVar "x", TMinus, TInt 3, TRParen, TMult, TLParen, TInt 4, TPlus, TInt 2, TMult, TInt 3, TRParen, TSeq, TVar "z", TAssign, TVar "x", TPlus, TVar "x", TMult, TLParen, TInt 2, TRParen, TSeq])
    putStrLn $ "Test 7: " ++ show (lexer "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == [TVar "i", TAssign, TInt 10, TSeq, TVar "fact", TAssign, TInt 1, TSeq, TWhile, TLParen, TNot, TLParen, TVar "i", TEqu, TInt 1, TRParen, TRParen, TDo, TLParen, TVar "fact", TAssign, TVar "fact", TMult, TVar "i", TSeq, TVar "i", TAssign, TVar "i", TMinus, TInt 1, TSeq, TRParen, TSeq])

testLexer :: String -> [Token] 
testLexer programCode = lexer programCode
