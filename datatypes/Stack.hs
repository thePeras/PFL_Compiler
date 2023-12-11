module Stack ( Stack, push, pop, top, createEmptyStack, isEmpty, stack2Str) where

data Stack a = Stk [a]

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack a -> a
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

createEmptyStack :: Stack a
createEmptyStack = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk [])= True
isEmpty (Stk _) = False

stack2Str :: Stack -> String
-- TODO
