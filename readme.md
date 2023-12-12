Low-level machine with configurations of the form (c, e, s),
c - list of instructions
e - evaluation stack
s - storage

Instructions of the machine:
- push-n
- add
- mult
- sub
- true
- false
- eq
- le,
- and
- neg
- fetch-x
- store-x
- noop
- branch(c1, c2)
- loop(c1, c2)


# TO DO LIST PART 1

[X] (a) Define a new type to represent the machine’s stack. The type must be named Stack.

[X] (b) Define a new type to represent the machine’s state. The type must be named State.

[X] (c) Implement the createEmptyStack function which returns an empty machine’s stack.
    ```createEmptyStack :: Stack```

[X] (d) Implement the createEmptyState function which returns an empty machine’s state.
    ```createEmptyState :: State```

[X] (e) Implement the ```stack2Str``` function which converts a stack given as input to a string.
    The string represents the stack as an ordered list of values, separated by commas
    and without spaces, with the leftmost value representing the top of the stack.
    ```stack2Str :: Stack → String```
    For instance, after executing the code [push−42, true, false], the string representing
    the stack is: False,True,42.

[X] (f) Implement the state2Str function which converts a machine state given as input to a string. The string represents the state as an list of pairs variable-value, separated by commas and without spaces, with the pairs ordered in alphabetical order of the variable name. Each variable-value pair is represented without spaces and using an ”=”.
    state2Str :: State → String
For instance, after executing the code [false, push − 3, true, store − var, store −a, store−someV ar], the string representing the state is: a=3,someVar=False,var=True.

[X] (g) Write an interpreter for programs in the same machine, which given a list of in-
structions (type defined as Code, i.e. type Code = [Inst]), a stack (type defined as
Stack) and that is initially empty, and a storage (type defined as State), runs the
list of instructions returning as ouput an empty code list, a stack and the output
values in the storage. This evaluation function must be declared as:
run :: (Code, Stack, State) → (Code, Stack, State)

# TO DO LIST PART 2

(...)