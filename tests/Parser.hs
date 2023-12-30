import Stack
import State

import Interpreter
import Compiler
import Parser

main :: IO ()
main = do
    putStrLn $ "Test 1: " ++ show (testParser "x := 5; x := x - 1;" == ("","x=4"))
    putStrLn $ "Test 2: " ++ show (testParser "x := 0 - 2;" == ("","x=-2"))
    putStrLn $ "Test 3: " ++ show (testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2"))
    putStrLn $ "Test 4: " ++ show (testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1"))
    putStrLn $ "Test 5: " ++ show (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2"))
    putStrLn $ "Test 6: " ++ show (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4"))
    putStrLn $ "Test 7: " ++ show (testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68"))
    putStrLn $ "Test 8: " ++ show (testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34"))
    putStrLn $ "Test 9: " ++ show (testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1"))
    putStrLn $ "Test 10: " ++ show (testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2"))
    putStrLn $ "Test 11: " ++ show (testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6"))
    putStrLn $ "Test 12: " ++ show (testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1"))

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (x:xs, stack, state) = run (newCode, newStack, newState)
  where (newCode, newStack, newState) = interpret x (xs, stack, state)

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)
