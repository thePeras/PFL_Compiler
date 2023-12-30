module Compiler where

import Interpreter (Inst(..), Code)

-- Definition of the Arithmetic expressions
data Aexp = Num Integer
          | Var String
          | AddExp Aexp Aexp
          | SubExp Aexp Aexp
          | MultExp Aexp Aexp
          deriving (Show)

-- Definition of the Boolean expressions
data Bexp = TrueExp
          | FalseExp
          | EquExp Aexp Aexp -- Equality
          | EquBoolExp Bexp Bexp  -- Boolean equality
          | LeExp Aexp Aexp
          | AndExp Bexp Bexp
          | NotExp Bexp
          deriving (Show)

-- Definition of the Statements expressions
data Stm = Assign String Aexp
         | If Bexp Stm Stm
         | While Bexp Stm
         | Seq Stm Stm -- Sequence: stm1; stm2
         deriving (Show)

-- Definition of the Program type
type Program = [Stm]

compA :: Aexp -> Code
compA (Num n)         = [Push n]
compA (Var x)         = [Fetch x]
compA (AddExp x y)    = compA y ++ compA x ++ [Add]
compA (SubExp x y)    = compA y ++ compA x ++ [Sub]
compA (MultExp x y)   = compA y ++ compA x ++ [Mult]

compB :: Bexp -> Code
compB TrueExp           = [Tru]
compB FalseExp          = [Fals]
compB (EquExp x y)      = compA y ++ compA x ++ [Equ]
compB (EquBoolExp x y)  = compB y ++ compB x ++ [Equ]
compB (LeExp x y)       = compA y ++ compA x ++ [Le]
compB (AndExp x y)      = compB y ++ compB x ++ [And]
compB (NotExp x)        = compB x ++ [Neg] 

compile :: Program -> Code
compile [] = []
compile (stm:stms) = compileStm stm ++ compile stms

compileStm :: Stm -> Code
compileStm (Assign x a)         = compA a ++ [Store x]
compileStm (Seq stm1 stm2)      = compileStm stm1 ++ compileStm stm2
compileStm (While cond stm)     = Loop (compB cond) (compileStm stm) : []
compileStm (If cond stm1 stm2)  = compB cond ++ [Branch (compileStm stm1) (compileStm stm2)]