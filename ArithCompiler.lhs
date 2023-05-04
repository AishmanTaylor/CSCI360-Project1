Project 1: Arith compiler
=========================

We won't spend much time in this course talking about compilers.  But
for this first project you will explore a very simple compiler for the
Arith language.

Preliminaries
-------------

First, download the files you will need:

* [ArithCompiler.lhs](ArithCompiler.lhs)
* [Parsing.hs](../../code/Parsing.hs)

`ArithCompiler.lhs` is the file you will edit for your project.  You
don't need to worry about `Parsing.hs`; just download it and put it in
the same directory as `ArithCompiler.lhs`.  You should be able to use
`repl.it` if you wish.  Just upload this file along with `Parsing.hs`
to your `repl.it` project and `:load ArithCompiler.lhs` as usual.

If you are using Haskell installed on your own computer, note that
depending on what version you have, you may need to start `ghci` with
a flag to tell it to use the `parsec` library, like so:

    ghci -package parsec ArithCompiler.lhs

First, some extensions and imports we will need for the parser; you
don't need to worry about these.

> {-# LANGUAGE GADTs #-}
> {-# OPTIONS_GHC -Wall #-}
> import Prelude hiding ((<$>), (<$), (<*>), (<*), (*>))
> import Parsing

AST and interpreter
-------------------

Here are the data types we used to represent Arith abstract syntax in
class, along with a simple interpreter.

> data Op where
>   Plus  :: Op
>   Minus :: Op
>   Times :: Op
>   deriving (Show, Eq)
>
> data Arith where
>   Lit :: Integer -> Arith
>   Bin :: Op -> Arith -> Arith -> Arith
>   deriving (Show)
>
> interp :: Arith -> Integer
> interp (Lit n)        = n
> interp (Bin op a1 a2) = interpOp op (interp a1) (interp a2)
>
> interpOp :: Op -> Integer -> Integer -> Integer
> interpOp Plus  = (+)
> interpOp Minus = (-)
> interpOp Times = (*)

A parser has been provided for your convenience, to help you test your
functions.  You can use the `readArith` function to parse concrete
Arith syntax into an AST. Caution: DO NOT USE `readArith` for
anything besides testing at the ghci prompt, since it crashes (via
the `error` function) when given any `String` that does not parse.

> readArith :: String -> Arith
> readArith s = case parse parseArith s of
>   Left  err -> error (show err)
>   Right a   -> a

For example, try evaluating `interp (readArith "(2+3)*4")`, which
should result in 20. This is much more convenient than typing `interp (Bin Times(Bin Plus (Lit 2) (Lit 3)) (Lit 4))`.

The abstract stack machine
--------------------------

Instead of compiling Arith programs to machine code, you will compile
them to an *abstract machine*.  An abstract machine is just like a
real machine except for the fact that it is imaginary.

Our imaginary machine is quite simple.  It keeps track of a list of
instructions to execute, and a stack of integers (recall that Haskell
lists can also be used as stacks).  There are four instructions it
knows how to execute:

+ `PUSH n`: given an integer `n`, push it on top of the stack.
+ `ADD`: pop the top two integers off the stack, add them, and push
  the result back on top of the stack.  The machine halts with an
  error if there are fewer than two integers on the stack.
+ `SUB`: pop the top two integers, subtract the topmost from the
  other, and push the result.
+ `MUL`: pop the top two integers, multiply them, and push the result.


1. **Make a data type called `Instruction` to represent the four stack
    machine instructions described above.**

- The imaginary machine first creates a ADT called Instruction with the following constructors:
-   PUSH takes in an integer and pushes it onto the stack, hence the Integer -> Instruction 
-   ADD will take two integers off the stack, add them together, and push them back onto the stack. 
    Due to the stack being both the input and output we can just call Instruction. 
-   SUB will take two integers off the stack, subtract them together, and push them back onto the stack. 
    Due to the stack being both the input and output we can just call Instruction.
-   MUL will take two integers off the stack, multiply them together, and push them back onto the stack. 
    Due to the stack being both the input and output we can just call Instruction.
-   deriving (Show, Eq) allows for this ADT to be printed to the console and to be compared using Show and Eq respectively. 
- This is to serve as a way to house all the valid operations our machien can undertake. 

> data Instruction where
>  PUSH :: Integer -> Instruction
>  ADD  :: Instruction
>  SUB  :: Instruction
>  MUL  :: Instruction
>  deriving (Show, Eq)

Our machine can also be in one of three states.  Each state may
additionally store some information.

+ `WORKING`: this state corresponds to normal operation of the
  machine.  It should contain a list of remaining instructions to
  execute and a stack of integers.

+ `DONE`: this state means there are no more instructions to execute.
  It should contain only the final stack.

+ `ERROR`: something has gone terribly, horribly wrong. In this state,
  the machine does not need to remember any instructions or stack.

2. **Make a data type called `MachineState` to represent the possible
   states of the machine, as described above.  Each different state
   should contain whatever information the machine needs to remember
   in that state.**

- The imaginary machine then creates a ADT called MachineState with the following constructors:
-   WORKING takes in a list of Instructions and a list of Integers to represent a machine in working order
    that is currently executing a 'program'. This is what gets us [Instruction] -> [Integer] -> MachineState. 
-   DONE takes in a list of Integers and represents a machine that has finished its assigned program. This is what gets us
    [Integer] -> MachineState.
-   ERROR represents a machine that has hit an error during execution and failed to run its current 'program'. 
-   deriving (Show, Eq) allows for this ADT to be printed to the console and to be compared using Show and Eq respectively. 

> data MachineState where
>  WORKING :: [Instruction] -> [Integer] -> MachineState
>  DONE    :: [Integer] -> MachineState
>  ERROR   :: MachineState
>  deriving (Show, Eq)

3. **Write a function `step :: MachineState -> MachineState` which
   executes a single step of the machine.  For example, in the
   `WORKING` state it should try executing the next instruction and
   return an appropriate next state for the machine.**

- step takes in a MachineState and will be giving a new MachineState as an output. 
- step accounts for each possible MachineState type. 
- As WORKING is the first constructor of MachineState it is checked first. 
-   WORKING has four four possible input Instruction: PUSH, ADD, SUB, MUL along with the stack of integers possible. 
-     step (WORKING (PUSH n : top) stack) simply takes in the PUSH Instruction, the Integer wishing to be pushed n, the location 
      of the top of the stack needed, and the stack of integers itself. The updated MachineState now has the pushed Integer on top
      and the remaining Instructions in the stack. 
-     step (WORKING (ADD : top) (x:y:stack)), (WORKING (SUB : top) (x:y:stack)), (WORKING (MUL : top) (x:y:stack)) all have similar design
      philisophy as they utilize the stack in the same way. They access the top of the stack, pop two numbers x and y off 
      the top of the stack, performs the associated operator on them (+,-, or *), and pushes them back onto the stack.
-   step (WORKING [] stack) is utilized when the stack has no more Instruction remaining. As the stack has been completed successfully
    instead of pushing to the top, the stack is returned with DONE as the final arguemnt. 
-   step _ is a final catch all MachineState that returns an ERROR if anything other than the above MachineState occur as an error
    most likely has happened. 

> step :: MachineState -> MachineState
> step (WORKING (PUSH n : top) stack) = WORKING top (n : stack)
> step (WORKING (ADD : top) (x:y:stack)) = WORKING top (x + y : stack)
> step (WORKING (SUB : top) (x:y:stack)) = WORKING top (y - x : stack)
> step (WORKING (MUL : top) (x:y:stack)) = WORKING top (x * y : stack)
> step (WORKING [] stack) = DONE stack
> step _ = ERROR

4. **Write `execute :: [Instruction] -> MachineState`, which takes a
   program and runs the machine (starting with an empty stack) until
   the machine won't run anymore (that is, it has reached a `DONE` or
   `ERROR` state).  (Hint: first write a helper function `steps ::
   MachineState -> MachineState`.)**

- steps takes in a MachineState and will be giving a new MachineState as output.
-   steps pattern matches the state across the possible Instruction which can influence the MachineState.
-     If there is an ERROR the MachineState is an ERROR.
-     If the state simply registers a Done result, the MachineState is a Done result.
-     If the state is WORKING and reaches the end of the stack, the MachineState is a Done stack.
-     If the state has at least one instruction then pattern matching must be conducted to determine what must be done. 
-       If the instruction is PUSH n it creates a new MachineState and returns the integer n on top of the stack and the remaining
        instructions in top. 
-       Like with step ADD, SUB, and MUL take on similar structuring with differing operators(+,-, or * respectively). If the 
        instruction recieved is ADD, SUB, or MUL then the stack is checked to see if it has two integers, if it does the appropriate
        operation is done and the result is pushed back onto the stack and a new MachineState is created. If any other possibility
        is present like there are not two integers in the stack, an error is returned. 
- execute takes in a list of Instruction and returns a MachineState. 
- It uses the steps function as a helper function and feeds the list into steps until the list reaches the end or an error is encountered. 

> steps :: MachineState -> MachineState
> steps state = case state of 
>   ERROR -> ERROR
>   DONE result -> DONE result
>   WORKING [] stack -> DONE stack
>   WORKING (instruction : top) stack -> 
>     case instruction of
>       PUSH n -> steps (WORKING top (n : stack))
>       ADD    -> case stack of
>         x:y:rest' -> steps (WORKING top (x + y : rest'))
>         _         -> ERROR
>       SUB -> case stack of
>         x:y:rest' -> steps (WORKING top (x - y : rest'))
>         _         -> ERROR
>       MUL -> case stack of
>         x:y:rest' -> steps (WORKING top (x * y : rest'))
>         _          -> ERROR
>
> execute :: [Instruction] -> MachineState
> execute instructions = steps (WORKING instructions [])

5. **Finally, write `run :: [Instruction] -> Maybe Integer`, which
   executes the program and then returns `Nothing` if the machine
   halted with an `ERROR` or an empty stack, or `Just` the top integer
   on the stack if the machine successfully finished and left at least
   one integer on the stack.**

- run takes in a list of Instruction and returns a Maybe Integer. 
- run pattern matches the various states of executing instructions possible. 
-   If execute returns ERROR then no Integer can be returned so Nothing is returned. 
-   If execute returns WORKING then the machine is still running and therefore no Integer can be returned so Nothing is returned too. 
-   If execute returns DONE but with an empty stack no Integer can be returned from an empty stack so Nothing is returned. 
-   If execute returns DONE with a successfully processed stack the top Integer is extracted and returned. 

> run :: [Instruction] -> Maybe Integer
> run instructions = case execute instructions of
>    ERROR       -> Nothing
>    WORKING _ _ -> Nothing
>    DONE []     -> Nothing
>    DONE (x:_)  -> Just x

The compiler
------------

Now that you have a working abstract machine, you can compile Arith
expressions into equivalent programs that run on the abstract machine.

6. **Write a function `compile` which takes an `Arith` and yields a
list of `Instruction`s.**

- compile takes in an Arith and returns a list of Instruction for running in the machine. 
- If compile is given just a number, in the form of Lit n, it returns a list of instructions containing PUSH n where 
  the n is the same as Lit n.
- If it recieves an operation, in the form of Bin op a1 a2, it will recursively call itself onto a1 and a2 until it hits Lit n 
  and PUSH n is added onto the list of Instruction. Then it pattern matches the operator from the given list of possibilities. 
-   Like with all the other previous examples Plus, Minus, and Times follow a similar structure. If op is Plus it appends ADD onto 
    the stack of Instruction, if op is Minus it appends SUB, and if it is Times, it appends MUL.

> compile :: Arith -> [Instruction]
> compile (Lit n) = [PUSH n]
> compile (Bin op a1 a2) =
>   compile a1 ++ compile a2 ++ case op of
>     Plus  -> [ADD]
>     Minus -> [SUB]
>     Times -> [MUL]

Of course, your compiler should output not just *any* list of
instructions!  It should output a program which, when run on the
abstract machine, successfully produces the same integer result as the
Arith interpreter would.  That is, for any `a :: Arith`,

```
run (compile a) == Just (interp a)
```

To test the above it will be convenient to write a function which
finally puts some of these things together:

7. **Write a function `exec :: String -> Maybe Integer` which takes a
   `String`, parses it, compiles the resulting `Arith`, and then
   `run`s the generated abstract machine instructions.  It should
   return `Nothing` if the given `String` does not parse.** (Do not
   call `readArith`, which crashes when given invalid input---instead,
   you should directly call `parse parseArith` and pattern-match on
   the output yourself.  Though you should not *call* `readArith`, you
   can and should use its implementation as an example.)

- exec takes in a String and returns a Maybe Integer
- exec takes String s and pattern matches it through the arith parser. 
-   If parsing fails it returns nothing. 
-   If parsing succeeds it passes the result to the compile function and then executes run on the result to get a Integer. 

> exec :: String -> Maybe Integer
> exec s =
>  case parse parseArith s of
>    Left _  -> Nothing
>    Right a -> run (compile a)


You should now be able to test that if `s` is any `String`, then `eval s == exec s`.

Level 1
-------

To complete this project to Level 1, do the above steps completely and
correctly. `eval s == exec s` must be true for all strings `s`.

Level 2
-------

To complete this project to Level 2, in addition to the requirements for Level 1:

- Ensure that your code uses [good Haskell
   style](https://kowainik.github.io/posts/2019-02-06-style-guide),
   for example:
    - Use `camelCase` for variable names.
    - Use informative but not-too-long variable names.
    - Vertically align `=` signs within a function definition.
    - Use consistent indentation (see above linked style guide).
        - Note in particular "The indentation of a line should not
           depend on the length of any identifier in preceding
           lines".

- Make sure your code is simplified as much as possible, for example,
  without redundant pattern-matching.

- Turn on `{-# OPTIONS_GHC -Wall #-}` and make sure your code generates no warnings.

- Write informative, grammatically correct comments explaining your
   code, its operation, and any choices you made along with the
   reasons for those choices.

Level 3
-------

Consider a different virtual machine with the following
characteristics:

- The machine has a (potentially infinite) memory of integer cells,
  addressed by consecutive natural numbers 0, 1, 2, ... The cells all
  start out with value 0.
- It also has a single "accumulator register" which can store a
  single integer.  The accumulator also starts as 0.
- The machine supports the following instructions:
    - `ACCUM n`: Set the value of the accumulator to the integer `n`.
    - `STORE a`: Store the current contents of the accumulator into the memory
      cell with address `a`.
    - `ADD a`: Add the contents of the memory cell at address `a` to
      the accumulator, updating the accumulator with the new value.
    - `SUB a`: Subtract the contents of the memory cell at address `a`
      from the accumulator, updating the accumulator with the new value.
    - `MUL a`: Multiply the contents of the memory cell at address `a`
      by the accumulator, updating the accumulator with the new value.

You should implement:

- A `run` function which executes a list of instructions for this
  abstract machine, and returns the final contents of the accumulator.
- A compiler which takes an `Arith` expression and compiles it to a
  list of instructions for this virtual machine.
- Be sure to include some examples showing that your compiler works
  correctly, that is, that your compiler is semantics-preserving:
  interpreting an `Arith` expression should give the same result as
  compiling and then running.

Parser
------

[Pay no attention to the man behind the curtain](https://www.youtube.com/watch?v=YWyCCJ6B2WE)...

> lexer :: TokenParser u
> lexer = makeTokenParser emptyDef
>
> parens :: Parser a -> Parser a
> parens = getParens lexer
>
> reservedOp :: String -> Parser ()
> reservedOp = getReservedOp lexer
>
> integer :: Parser Integer
> integer = getInteger lexer
>
> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer
>
> parseAtom :: Parser Arith
> parseAtom = Lit <$> integer <|> parens parseExpr
>
> parseExpr :: Parser Arith
> parseExpr = buildExpressionParser table parseAtom
>   where
>     -- Each list of operators in the table has the same precedence, and
>     -- the lists are ordered from highest precedence to lowest.  So
>     -- in this case '*' has the highest precedence, and then "+" and
>     -- "-" have lower (but equal) precedence.
>     table = [ [ binary "*" (Bin Times) AssocLeft ]
>             , [ binary "+" (Bin Plus)  AssocLeft
>               , binary "-" (Bin Minus) AssocLeft
>               ]
>             ]
>
>     binary name fun assoc = Infix (reservedOp name >> return fun) assoc
>
> parseArith :: Parser Arith
> parseArith = whiteSpace *> parseExpr <* eof
>
> eval :: String -> Maybe Integer
> eval s = case parse parseArith s of
>            Left _  -> Nothing
>            Right a -> Just (interp a)
