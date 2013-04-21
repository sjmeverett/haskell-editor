{-******************************************************************-}
{- FOUL Language                                                    -}
{-******************************************************************-}

module FOUL where

{-
In this file, we'll implement a small functional programming language,
the First-Order Untyped Language, FOUL. The idea is to explore how
functional languages work, conceptually. Of course, as we'll be
implementing FOUL in the functional language Haskell, it's more
reinforcement than a return to first principles.

We'll need some first-order values for FOUL programs to manipulate.
They're called "first-order" because they contain no functions. A
function which takes first-order inputs is "second-order", and so it
goes. But we're not going to chuck functions around until next week,
so here goes.
-}


{-******************************************************************-}
{- FOUL Overview                                                    -}
{-******************************************************************-}

{-
A FOUL program consists of a bunch of named functions, each of which
is defined by some program lines.
-}

type FName = String
type Prog = [(FName, [Line])]

{-
A function line has a bunch of patterns on the left, and an expression
to evaluate if those patterns match the function's inputs.
-}

type Line = ([Pat], Expr)


{-******************************************************************-}
{- FOUL Values                                                      -}
{-******************************************************************-}

{- Let's see what FOUL values are -}

type CName = String
data Val
  = VC CName [Val]  -- a constructor with 0 or more subvalues
  deriving Show

{- Let's have some examples. -}

{- We might make truth-values as constants: -}

true :: Val
true = VC "True" []

false :: Val
false = VC "False" []

{- We might make numbers by counting from zero: -}

zero :: Val
zero = VC "Zero" []  -- zero has no subvalues

suc :: Val -> Val
suc n = VC "Suc" [n]

{- We might make lists with nil and cons: -}

nil :: Val
nil = VC "[]" []

cons :: Val -> Val -> Val
cons x xs = VC ":" [x, xs]

{- We might make pairs like this: -}

pair :: Val -> Val -> Val
pair a b = VC "Pair" [a, b]

{- We might make binary trees with labelled nodes: -}

leaf :: Val
leaf = VC "Leaf" []

node :: Val -> Val -> Val -> Val
node left label right = VC "Node" [left, label, right]

{-
Here's an example, built in ghci, then creatively rewhitespaced.

*FOUL> node (node leaf zero leaf) (suc zero) leaf
VC "Node"
  [  VC "Node" 
       [  VC "Leaf" []
       ,  VC "Zero" []
       ,  VC "Leaf" []
       ]
  ,  VC "Suc" [  VC "Zero" []]
  ,  VC "Leaf" []
  ]
-}

{-******************************************************************-}
{- FOUL Patterns                                                    -}
{-******************************************************************-}

{-
A pattern is like a value, except that some parts have been abstracted
away by variables. That is, a pattern is a value template.
-}

type VName = String
data Pat
  =  PV VName
  |  PC CName [Pat]
  deriving Show

{-
We can try to figure out if a value matches a pattern. If so, we can
build an environment showing how the pattern variables correspond to
pieces of the value matched.
-}

type Env = [(VName, Val)]

{- Let's write a matching algorithm. -}

match :: Pat -> Val -> Maybe Env
match (PV x) v = Just [(x, v)]
match (PC c ps) (VC c' vs)
  | c == c'    = matches ps vs
  | otherwise  = Nothing

matches :: [Pat] -> [Val] -> Maybe Env
matches [] [] = Just []
matches (p : ps) (v : vs) = case match p v of
  Nothing -> Nothing
  Just e1 -> case matches ps vs of
    Nothing -> Nothing
    Just e2 -> Just (e1 ++ e2)
matches _ _ = Nothing


{-******************************************************************-}
{- FOUL Expressions                                                 -}
{-******************************************************************-}

{- Expressions are built as follows -}

data Expr
  = EC CName [Expr]   -- just like values
  | EV VName          -- from variables (coming from patterns)
  | EA FName [Expr]   -- by applying functions (from the program)
  deriving Show

{-
We'd better check that we can make constant expressions from values.
-}

constant :: Val -> Expr
constant (VC c vs) = EC c (map constant vs)

{-
Let's evaluate expressions: we'll need a program to interpret
functions and an environment to interpret variables.
-}

eval :: Prog -> Env -> Expr -> Val
eval fs gam (EC c es)  = VC c (map (eval fs gam) es)
eval fs gam (EV x)     = fetch x gam
eval fs gam (EA f es)  = runfun (fetch f fs) (map (eval fs gam) es)
  where
    runfun :: [Line] -> [Val] -> Val
    runfun ((ps, e) : ls) vs = case matches ps vs of
      Nothing    -> runfun ls vs
      Just gam'  -> eval fs gam' e

{- We need that looker-upper function. -}

fetch :: String -> [(String, x)] -> x
fetch x ((y, v) : gam)
  | x == y     = v
  | otherwise  = fetch x gam


{-******************************************************************-}
{- FOUL Example                                                     -}
{-******************************************************************-}

{- I've written a program for you, to show how it's done. -}

plusProg :: Prog
plusProg =
  [  ("plus",
       [  ([PC "Zero" [],       PV "y"], EV "y")
       ,  ([PC "Suc" [PV "x"],  PV "y"], EC "Suc" [EA "plus" [EV "x", EV "y"]])
       ])
  ]

{- And here's a test example. -}

testPlus :: Val
testPlus =
  eval plusProg []
    (EA "plus"  [  EC "Suc" [EC "Suc" [EC "Zero" []]]
                ,  EC "Suc" [EC "Suc" [EC "Zero" []]]
                ])
