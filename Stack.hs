module Stack (Stack, StackElement(..), createEmptyStack, stack2Str) where

-- Definition of the StackElement type
data StackElement = IntElement Integer | BoolElement Bool
  deriving (Show, Eq)

-- Definition of the Stack type
type Stack = [StackElement]

createEmptyStack :: Stack
createEmptyStack = []

showValue :: StackElement -> String
showValue (IntElement x) = show x
showValue (BoolElement x) = show x

stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [x] = showValue x
stack2Str (x:xs) = showValue x ++ "," ++ stack2Str xs
