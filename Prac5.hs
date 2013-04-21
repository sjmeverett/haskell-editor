{- Practical 5: Looping Hutton's Chatterboxes -}

module Prac5 where

import Control.Applicative
import Control.Monad

{-
A "chatterbox" is a process which can perform input/output side-effects.
In this question, the inputs and outputs are just integer values. We
shall be adding basic input/output capacity to Hutton's primitive language
of integers with addition.
-}

{-
Firstly, we must model the side-effect capabilities by constructing a
suitable instance of Monad.
-}

{-
(a) Extend the following datatype so that it represents strategies for
computation with side-effecting operations

   input   :: () -> Int     -- receive a number from the input
   output  :: Int -> ()     -- send a number to the output
-}

data IntIO x
  = Return x
  | Input (Int -> IntIO x)
  | Output Int (IntIO x)

--(2 marks)

{-
(b) Show that your IntIO is indeed an instance of Monad by extending
the following instance declaration.
-}

instance Monad IntIO where
  return = Return
  Return x >>= f = f x
  Input  p >>= f = Input $ \i -> p i >>= f
  Output i p >>= f = Output i (p >>= f)

--(2 marks)

{-
Instances of Applicative and Functor have been provided for you, in the
standard way.
-}

instance Applicative IntIO where
  pure = return
  (<*>) = ap

instance Functor IntIO where
  fmap = (<*>) . pure

{-
(c) Check that you can implement input and output as operations within
the IntIO monad.
-}

input :: () -> IntIO Int
input v = Input $ \i -> Return i

output :: Int -> IntIO ()
output o = Output o (Return ())

--(2 marks)

{-
(d) Show how to interpret one of your computation strategies as a function
from a list of inputs to a list of outputs by extending the following.
-}

runIntIO :: IntIO x -> [Int] -> [Int]
runIntIO (Return _) _ = []
runIntIO (Input p) (x:xs) = runIntIO (p x) xs
runIntIO (Output i p) xs = i : (runIntIO p xs)

--(2 marks)

{-
(e) Develop a "chaining" operator, p >>> q, which allows q to get its
inputs from the outputs of p until p terminates, from which point q
should get its inputs externally. You should treat p's inputs and q's
outputs as external. Moreover, you should prioritize the execution of
p >>> q in favour of the "output end", q, evolving p only when q wants
input.
-}

(>>>) :: IntIO x -> IntIO y -> IntIO y
p >>> Return y = Return y
p >>> Output i q = Output i (p >>> q)

Output i p >>> Input q = p >>> q i

Input p >>> Input q = do
    i <- input ()
    p i >>> Input q
    
Return _ >>> q = q

--(3 marks)

{-
If you have successfully prioritized output, it should make sense to
loop a process, feeding its output back to its input, as well as to
the world. Such a looped process should work just fine, provided it
never demands an input for which it has not already delivered a value
via the output. Here's how to build such a loop.
-}

loopy :: IntIO x -> IntIO x
loopy q = p where p = p >>> q

{-
Now here's Hutton's language of integers and addition, extend with
input and output capability, and a sequential composition operator.
-}

data HIO
  = V Int         -- integer values
  | HIO :+: HIO   -- addition
  | Echo HIO      -- Echo e evaluates e, then outputs and returns e's value
  | Fetch         -- fetch gets a value from the input
  | HIO :/: HIO   -- e1 :/: e2, runs e1, drops its value, then runs e2

infixr 4 :/:
infixr 5 :+:

{-
(f) Write an interpreter for this language, modelling the effects in
your IntIO monad.
-}

hio :: HIO -> IntIO Int
hio (V i) = Return i

hio (p :+: q) = do
    a <- hio p
    b <- hio q
    return (a + b)
    
hio (Echo h) = do
    e <- hio h
    output e
    return e
    
hio (Fetch) = do
    input ()
    
hio (p :/: q) = do
    hio p
    hio q

--(5 marks)

{-
(g) Using recursion, construct an infinite expression which echoes each
input, incremented by 1, to its output. For example, if the input is
1,2,3,... the output should be 2,3,4,...
-}

myLoop :: HIO
myLoop = Echo (Fetch :+: V 1) :/: myLoop

--(2 marks)

{-
(h) Show how to use myLoop to construct an expression which behaves
like a counter when you feed its output back into its input, like this:
-}

myCounter :: [Int]
myCounter = runIntIO (loopy (hio myProg)) []

{-
You should ensure that

  take 5 myCounter = [0, 1, 2, 3, 4]
-}

myProg :: HIO
myProg = Echo (V 0) :/: myLoop

--(2 marks)
