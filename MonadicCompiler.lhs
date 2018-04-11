G52AFP Coursework 2 - Monadic Compiler

William Michael Hickling | James Simon Morris
psywmh@nottingham.ac.uk  | psyjsmor@nottingham.ac.uk
-----------------------------------------------------

Program is either an assignment, conditional, while loop or a sequence of
programs.

> data Prog = Assign Name Expr
>           | If Expr Prog Prog
>           | While Expr Prog
>           | Seqn [Prog]

An expression is either an integer value, a variable name, or the application
of an operator or two argument expressions

> data Expr = Val Int | Var Name | App Op Expr Expr
> type Name = Char
> data Op = Add | Sub | Mul | Div

Logical value False is represented as zero, true is any other integer.

Function to calculate factorial of non-negative integer

> fac :: Int -> Prog
> fac n = Seqn [Assign 'A' (Val 1),
>               Assign 'B' (Val n),
>               While (Var 'B') (Seqn
>                  [Assign 'A' (App Mul (Var 'A')(Var 'B')),
>                   Assign 'B' (App Sub (Var 'B')(Val (1))])]

> type Stack = [Int]
>
> type Mem = [(Name, Int)]

There are a list of instructions for the compiler. They either push an integer
onto the stack, push the value of a variable, pops the top of the stack into
a variable, performs an operation on the stack, jumps to a label, pops the
stack and jumps if the value is zero, or is simply a label.

> type Code = [Inst]
>
> data Inst = PUSH Int
>           | PUSHV Name
>           | POP Name
>           | DO Op
>           | JUMP Label
>           | JUMPZ Label
>           | LABEL Label
>           deriving Show
>
> type Label = Int
