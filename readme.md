# Compiler

>**Project**
><br />
>Course Unit: [Programação Funcional e em Lógica](https://sigarra.up.pt/feup/en/UCURR_GERAL.FICHA_UC_VIEW?pv_ocorrencia_id=520329 "Functional and Logic Programming"), 3rd year 
><br />
>Course: **Informatics and Computer Engineering** 
><br />
> Faculty: **FEUP** (Faculty of Engineering of the University of Porto)
><br />
> Report: [report.pdf](./report.pdf)
><br/>
> Project evaluation: **_**/20


## Project Goals

1. Implement a **low-level stack-based architecture machine** with simple instructions like push, add, mult, eq, le,  fetch-x, store-x, branch(c1, c2), loop(c1, c2), etc. The **interpreter** uses a configuration of the form (c, e, s) where c is a list of instructions (or code) to be executed, e is the evaluation stack, and s is the storage. We use the evaluation stack to evaluate arithmetic and boolean expressions.

2. Design a compiler for a small imperative programming language with arithmetic and boolean expressions, assignments, sequences of statements, if-then-else statements, and while loops. 
The compiler translates programs written in this language into lists of machine instructions. 
The task includes defining **data structures in Haskell** to represent expressions and statements, developing a **compiler** function, and creating a **parser** to transform string representations of programs into corresponding imperative language structures. The parser handles syntax rules, variable naming, operator precedence in arithmetic and boolean expressions, and enforces proper execution order of operations by using **tokens** produced by a **lexer**.

You can read more about the goals in the [project assignment](./Assignment.pdf).
Implementation details can be found in the [report](./report.pdf) and the [source code](./src/).