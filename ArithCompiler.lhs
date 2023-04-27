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
>
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
should result in 20. This is much more convenient than typing `interp (Bin Times
(Bin Plus (Lit 2) (Lit 3)) (Lit 4))`.

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

> data MachineState where
>  WORKING :: [Instruction] -> [Integer] -> MachineState
>  DONE    :: [Integer] -> MachineState
>  ERROR   :: MachineState
>  deriving (Show, Eq)

3. **Write a function `step :: MachineState -> MachineState` which
   executes a single step of the machine.  For example, in the
   `WORKING` state it should try executing the next instruction and
   return an appropriate next state for the machine.**

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

5. **Finally, write `run :: [Instruction] -> Maybe Integer`, which
   executes the program and then returns `Nothing` if the machine
   halted with an `ERROR` or an empty stack, or `Just` the top integer
   on the stack if the machine successfully finished and left at least
   one integer on the stack.**


The compiler
------------

Now that you have a working abstract machine, you can compile Arith
expressions into equivalent programs that run on the abstract machine.

6. **Write a function `compile` which takes an `Arith` and yields a
list of `Instruction`s.**

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

You should now be able to test that if `s` is any `String`, then `eval
s == exec s`.

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
