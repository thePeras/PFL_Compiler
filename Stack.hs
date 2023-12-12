module Stack (Stack, push, pop, top, createEmptyStack, isEmpty, stack2Str, stack2StrRec) where

data Stack = Stk [Integer]

push :: Integer -> Stack -> Stack
push x (Stk xs) = Stk (x:xs)

pop :: Stack -> Stack
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack -> Integer
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

createEmptyStack :: Stack
createEmptyStack = Stk []

isEmpty :: Stack -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _) = False

stack2Str :: Stack -> String
stack2Str (Stk stack) = foldr (\x acc -> show x ++ "," ++ acc) "" stack

stack2StrRec :: Stack -> String
stack2StrRec (Stk []) = ""
stack2StrRec (Stk (x:xs)) = show x ++ "," ++ stack2StrRec (Stk xs)
