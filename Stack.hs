module Stack (Stack, createEmptyStack, push, pop, top, top1, top2, isEmpty, stack2Str) where

-- Definition of the StackElement type
data StackElement = StackElement Integer | StackElementBool Bool
  deriving (Show, Eq)

-- Definition of the Stack type
data Stack = Stack [StackElement]
  deriving (Show, Eq)


createEmptyStack :: Stack
createEmptyStack = Stack []

push :: StackElement -> Stack -> Stack
push x (Stack xs) = Stack (x:xs)

pop :: Stack -> Stack
pop (Stack (_:xs)) = Stack xs
pop _ = error "Stack.pop: empty stack"

top :: Stack -> StackElement
top (Stack (x:_)) = x
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
isEmpty (Stack []) = True
isEmpty (Stack _) = False

stack2Str :: Stack -> String
stack2Str (Stack []) = ""
stack2Str (Stack [x]) = show x
stack2Str (Stack (x:xs)) = show x ++ "," ++ stack2Str (Stack xs)