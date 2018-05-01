G52AFP Coursework 2 - Monadic Compiler

William Michael Hickling | James Simon Morris
psywmh@nottingham.ac.uk  | psyjsmor@nottingham.ac.uk
-----------------------------------------------------

> import Data.List

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
>                 [Assign 'A' (App Mul (Var 'A')(Var 'B')),
>                  Assign 'B' (App Sub (Var 'B')(Val (1))])]

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

State Transformer Monad
-----------------------

Need to use data mechanism to make ST into an instance of a class.

> data ST a = S(State -> (a,State))

Removes the dummy constructor

> apply            :: ST a -> State -> (a, State)
> apply (S f) x    =  f x

return             :: a -> ST a
(>>=)              :: ST a -> (a -> ST b) -> ST b

> type State       = Label
>
> fresh            :: ST Int
>
> app              :: (State -> State) -> ST State
>
> run              :: ST a -> State -> a

Compiler Code
-------------

Compiles program and expressions, assigning fresh labels when needed.

> comp             :: Prog -> Code
> comp p           =  run (compProg p) 0

Compile Program

> compProg                :: Prog -> ST Code
> compProg(Seqn[])        =  return []
> compProg(Seqn(x:xs))    =  do cp      <- compProg x
>                                cpSeq  <- compProg (Seqn xs)
>                                return (cp ++ cpSeq)
> compProg(Assign n expr) =  return (compExp expr ++ [POP n])
>
> compProg(If expr p1 p2) =  do l       <- fresh
>                               l'      <- fresh
>                               TrueSeq <- compProg p1
>                               FalSeq  <- compProg p2
>                               return(compExp expr ++ [JUMPZ l] ++ TrueSeq ++
>                                     [JUMP l', LABEL l] ++ FalSeq ++
>                                     [LABEL l'])
>
> compProg(While expr p1) =  do l       <- fresh
>                               l'      <- fresh
>                               cpSeq   <- compProg s
>                               return([LABEL l] ++ compExp expr ++ [JUMPZ l']
>                                     ++ cpSeq ++ [JUMP 0, LABEL l'] )

Compile Expressions

> compExp          :: Expr -> Code

Optimise expressions by removing integer operations

> optOp            :: Op -> Int -> Int -> Int

Code Execution
--------------

Program counter

> type PC = Int
>
> exec             :: Code -> Mem

Runs the program execution, keeping track of memory, the stack and program
counter

> ex               :: Code -> Mem -> Stack -> PC -> Mem

Removes all old duplicates from memory

> clear            :: Mem -> Mem

Retrieves a variable from memory

> getVar           :: Name -> Mem -> Int

Runs arithmetic operations

> doOp             :: Op -> [Int] -> Int

Get the program counter at a given label

> getPC            :: Code -> Label -> Int -> PC
