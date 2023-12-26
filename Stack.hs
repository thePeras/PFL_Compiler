module Stack (Stack, push, pop, top, top1, top2, createEmptyStack, isEmpty, stack2Str) where

data Stack = Stk [Integer]

push :: Integer -> Stack -> Stack
push x (Stk xs) = Stk (x:xs)

pop :: Stack -> Stack
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack -> Integer
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

-- Returns a tuple (x, stackWithoutX)
--  x: top of the stack
--  stackWithoutX: stack without the top element
top1 :: Stack -> (Integer, Stack)
top1 stack = (x, stackWithoutX)
  where x = top stack
        stackWithoutX = pop stack

-- Returns a tuple (x, y, stackWithoutXY)
--  x: top of the stack
--  y: second element of the stack
--  stackWithoutXY: stack without the top two elements
top2 :: Stack -> (Integer, Integer, Stack)
top2 stack = (x, y, stackWithoutXY)
  where x = top stack
        y = top (pop stack)
        stackWithoutXY = pop (pop stack)

createEmptyStack :: Stack
createEmptyStack = Stk []

isEmpty :: Stack -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _) = False

stack2Str :: Stack -> String
stack2Str (Stk []) = ""
stack2Str (Stk [x]) = show x
stack2Str (Stk (x:xs)) = show x ++ "," ++ stack2Str (Stk xs)
