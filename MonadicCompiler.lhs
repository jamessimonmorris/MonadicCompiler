G52AFP Coursework 2 - Monadic Compiler

William Michael Hickling | James Simon Morris
psywmh@nottingham.ac.uk  | psyjsmor@nottingham.ac.uk
--------------------------------------------------------------------------------

> import Data.List

Imperative language:

> data Prog = Assign Name Expr
>           | If Expr Prog Prog
>           | While Expr Prog
>           | Seqn [Prog]
>             deriving Show
>
> data Expr = Val Int | Var Name | App Op Expr Expr
>             deriving Show
>
> type Name = Char
>
> data Op   = Add | Sub | Mul | Div
>             deriving Show

Factorial example:

> fac :: Int -> Prog
> fac n = Seqn [Assign 'A' (Val 1),
>               Assign 'B' (Val n),
>               While (Var 'B') (Seqn
>                  [Assign 'A' (App Mul (Var 'A') (Var 'B')),
>                   Assign 'B' (App Sub (Var 'B') (Val (1)))])]

Virtual machine:

> type Stack = [Int]
>
> type Mem   = [(Name,Int)]
>
> type Code  = [Inst]
> 
> data Inst  = PUSH Int
>            | PUSHV Name
>            | POP Name
>            | DO Op
>            | JUMP Label
>            | JUMPZ Label
>            | LABEL Label
>              deriving Show
> 
> type Label = Int

State Monad:

> type State = Label
>
> newtype ST a = S (State -> (a, State))
>
> app :: ST a -> State -> (a,State)
> app (S st) x 	=  st x
>
> instance Functor ST where
>    -- fmap :: (a -> b) -> ST a -> ST b
>    fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))
>
> instance Applicative ST where
>    -- pure :: a -> ST a
>    pure x = S (\s -> (x,s))
>
>    -- (<*>) :: ST (a -> b) -> ST a -> ST b
>    stf <*> stx = S (\s ->
>       let (f,s')  = app stf s
>           (x,s'') = app stx s' in (f x, s''))
>
> instance Monad ST where
>    -- return :: a -> ST a
>    return x = S (\s -> (x,s))
>
>    -- (>>=) :: ST a -> (a -> ST b) -> ST b
>    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

--------------------------------------------------------------------------------

Compiles program and expressions, assigining fresh labels when needed

> comp                             :: Prog -> Code
> comp p                           =  run (compProg p) 0
>
> fresh                            :: ST Int
> fresh                            =  apply (+1)
>
> apply                              :: (State -> State) -> ST State
> apply f                            =  S (\n -> (n, f n))
>
> run                              :: ST a -> State -> a
> run p s                          =  fst (app p s) 

Compile program

> compProg                     :: Prog ->  ST Code
>
> compProg (Seqn [])           =  return []
>
> compProg (Seqn (x:xs))       =  do cx   <- compProg x
>                                    cSeq <- compProg (Seqn xs)
>                                    return (cx ++ cSeq) 
>
> compProg (Assign n xpr)          =  return (compExpr xpr ++ [POP n])
>
> compProg (While xpr s)           =  do l    <- fresh
>                                        l'   <- fresh
>                                        cSeq <- compProg s
>                                        return ([LABEL l] ++ compExpr xpr ++ [JUMPZ l'] ++ cSeq ++ [JUMP 0, LABEL l'])
>
> compProg (If xpr p1 p2)          =  do l     <- fresh
>                                        l'    <- fresh
>                                        cTSeq <- compProg p1 -- true sequence compiled
>                                        cFSeq <- compProg p2 -- false sequence compiled
>                                        return (compExpr xpr ++ [JUMPZ l] ++ cTSeq ++ [JUMP l', LABEL l] ++ cFSeq ++ [LABEL l'])

Compile separate expressions

> compExpr                          :: Expr -> Code
>
> compExpr (Val n)                  =  [PUSH n]
>
> compExpr (Var v)                  =  [PUSHV v]
>
> compExpr (App op (Val x) (Val y)) =  [PUSH (optExpr op x y)]
>
> compExpr (App op xp1 xp2)         =  compExpr xp1 ++ compExpr xp2 ++ [DO op]

Optimise expressions by removing integer operations

> optExpr                            :: Op -> Int -> Int -> Int
> optExpr Add x y                    =  (x + y)
> optExpr Sub x y                    =  (y - x)
> optExpr Mul x y                    =  (x * y)
> optExpr Div x y                    =  (y `div` x)

Code execution
---------------
Program Counter

> type PC = Int    
>     
> exec                             :: Code -> Mem
> exec c                           =  reverse (ex c [] [] 0)

Get the program counter at a given label

> getPC                            :: Code -> Label -> Int -> PC
> getPC code l n                   =  case (code!!n) of
>                                       LABEL l' -> if l == l' then n else getPC code l (n+1)
>                                       _        -> getPC code l (n+1) 

Runs the program execution, keeping track of memory, the stack and program counter

> ex                               :: Code -> Mem -> Stack -> PC -> Mem
>
> ex code mem stack pc             =  if pc < length code then
>                                        case (code!!pc) of
>                                          PUSH n     -> ex code mem (n:stack) (pc+1)
>                                          PUSHV v    -> ex code mem ((getVal v mem) : stack) (pc+1)
>                                          POP v      -> ex code ((v, head stack):mem) (tail stack) (pc+1)
>                                          DO op      -> ex code mem ((doOp op (take 2 stack)) : stack) (pc+1)
>                                          LABEL l    -> ex code mem stack (pc+1)
>                                          JUMP l     -> ex code mem stack (getPC code l 0)
>                                          JUMPZ l    -> ex code mem stack zPC
>                                                        where
>                                                           zPC = if (head stack) == 0 then (getPC code l 0) else (pc+1) 
>                                     else clear mem

Retrieves a value from memory

> getVal                           :: Name -> Mem -> Int
> getVal n ((v, i) : xs)           =  if v == n then i else getVal n xs

Removes all old duplicates from memory

> clear                            :: Mem -> Mem
> clear mem                        =  nubBy (\(v, i) (v', i') -> v == v') mem

Runs arithmetic operations

> doOp                             :: Op -> [Int] -> Int
> doOp Add (x:y:xs)                =  y + x
> doOp Sub (x:y:xs)                =  y - x
> doOp Mul (x:y:xs)                =  y * x
> doOp Div (x:y:xs)                =  y `div` x
