module Stack (Stack, createEmptyStack, push, pop, top, top1, top2, isEmpty, showValue, stack2Str) where

-- Definition of the StackElement type
data StackElement = IntElement Integer | BoolElement Bool
  deriving (Show, Eq)

-- Definition of the Stack type
data Stack = Stk [StackElement]
  deriving (Show, Eq)


createEmptyStack :: Stack
createEmptyStack = Stk []

push :: StackElement -> Stack -> Stack
push x (Stk xs) = Stk (x:xs)

pop :: Stack -> Stack
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack -> StackElement
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

-- Returns a tuple (x, stackWithoutX)
--  x: top of the stack
--  stackWithoutX: stack without the top element
top1 :: Stack -> (StackElement, Stack)
top1 stack = (x, stackWithoutX)
  where x = top stack
        stackWithoutX = pop stack


-- Returns a tuple (x, y, stackWithoutXY)
--  x: top of the stack
--  y: second element of the stack
--  stackWithoutXY: stack without the top two elements
top2 :: Stack -> (StackElement, StackElement, Stack)
top2 stack = (x, y, stackWithoutXY)
  where x = top stack
        y = top (pop stack)
        stackWithoutXY = pop (pop stack)

isEmpty :: Stack -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _) = False

showValue :: StackElement -> String
showValue (IntElement x) = show x
showValue (BoolElement x) = show x

stack2Str :: Stack -> String
stack2Str (Stk []) = ""
stack2Str (Stk [x]) = showValue x
stack2Str (Stk (x:xs)) = showValue x ++ "," ++ stack2Str (Stk xs)
