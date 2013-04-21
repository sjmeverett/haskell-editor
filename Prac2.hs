{-
CS410 Advanced Functional Programming
Autumn 2010
-}

{- Practical 2: Fair is Foul and Foul is Fair -}

module Prac2 where

{-
You will also need the file FOUL.lhs, which contains the FOUL
interpreter we developed in lectures.
-}

import FOUL

{-****************************************************************-}
{- Your Identity                                                  -}
{-****************************************************************-}

{- Put your name instead of Harry's in the following String! -}

myName :: String
myName = "Stewart MacKenzie-Leigh"


{-
Your mission is in two parts: firstly to write programs which
work in FOUL, and secondly to write programs which break FOUL.
-}


{-****************************************************************-}
{- Part One --- Fair is Foul                                      -}
{-****************************************************************-}

{-
Construct a sorting algorithm in FOUL by adding functions to
the following empty program.
-}

sortProg :: Prog
sortProg = [
    --lessThan _ Zero = False
    --lessThan Zero _ = True
    --lessThan (Suc x) (Suc y) = lessThan x y
        ("lessThan", 
        [
            ([PV "x", PC "Zero" []], EC "False" []),
            ([PC "Zero" [], PV "y"], EC "True" []),
            ([PC "Suc" [PV "x"], PC "Suc" [PV "y"]], EA "lessThan" [EV "x", EV "y"])
        ]),
        
    --ifThenElse True t f = t
    --ifThenElse False t f = f
        ("ifThenElse",
        [
            ([PC "True" [], PV "t", PV "f"], EV "t"),
            ([PC "False" [], PV "t", PV "f"], EV "f")
        ]),
        
    --insert n Leaf = Node Leaf n Leaf
    --insert n (Node l x r) = ifThenElse (lessThan n x) (Node (insert n l) x r) (Node l x (insert n r))
        ("insert",
        [
            ([PV "n", PC "Leaf" []], EC "Node" [EC "Leaf" [], EV "n", EC "Leaf" []]),
            ([PV "n", PC "Node" [PV "l", PV "x", PV "r"]], EA "ifThenElse" [EA "lessThan" [EV "n", EV "x"], EC "Node" [EA "insert" [EV "n", EV "l"], EV "x", EV "r"], EC "Node" [EV "l", EV "x", EA "insert" [EV "n", EV "r"]]])
        ]),
        
    --buildTree t [] = t
    --buildTree t (x:xs) = buildTree (insert x t) xs
        ("buildTree",
        [
            ([PV "t", PC "[]" []], EV "t"),
            ([PV "t", PC ":" [PV "x", PV "xs"]], EA "buildTree" [EA "insert" [EV "x", EV "t"], EV "xs"])
        ]),
        
    --append [] y = y
    --append (x:xs) y = x : (append xs y)
        ("append",
        [
            ([PC "[]" [], PV "y"], EV "y"),
            ([PC ":" [PV "x", PV "xs"], PV "y"], EC ":" [EV "x", EA "append" [EV "xs", EV "y"]])
        ]),
        
   --flatten Leaf = []
   --flatten (Node l x r) = append (flatten l) (x:(flatten r))
        ("flatten",
        [
            ([PC "Leaf" []], EC "[]" []),
            ([PC "Node" [PV "l", PV "x", PV "r"]], EA "append" [EA "flatten" [EV "l"], EC ":" [EV "x", EA "flatten" [EV "r"]]])
        ]),
        
    --sort xs = flatten (buildTree Leaf xs)
        ("sort",
        [
            ([PV "xs"], EA "flatten" [EA "buildTree" [EC "Leaf" [], EV "xs"]])
        ])
    ]

{-
I'm hoping that the following example (and others of its kind)
might produce sensible output eventually.
-}

mySort :: Val
mySort = eval sortProg []
  (EA "sort" [ EC ":" [ EC "Suc" [EC "Suc" [EC "Zero" []]]
              , EC ":" [ EC "Suc" [EC "Zero" []]
               , EC ":" [ EC "Suc" [EC "Suc" [EC "Suc" [EC "Zero" []]]]
                , EC ":" [ EC "Zero" []
                 , EC "[]" []
             ]]]]])

{-
Suggested steps to a solution

(1) Develop a comparison function, e.g. "less than";
(2) write the function to insert a number into a binary search tree;
(3) implement if-then-else as a function
(4) write the function to build a binary search tree up from empty
      by inserting the elements of a list, one by one, using comparison
      to keep the tree in order;
(5) write the function to append two lists;
(6) write the function to flatten a binary search tree, getting a list;
(7) make sort the function which builds a binary search tree from its input
      and then flattens it.

Feel free to choose a different algorithm, but make it sort.
-}

{-****************************************************************-}
{- Part Two --- Foul is Fair                                      -}
{-****************************************************************-}

{-
Show that my implementation of FOUL deserves to be described as "foul",
in that it can easily be persuaded to fail to return a value. More
particularly, implement a program
-}

badProg :: Prog
badProg = [
        ("addOne", [
            ([PV "x"], EC "Suc" [EV "x"])
        ]),
        
        ("infinite", [
            ([], EA "infinite" [])
        ])
    ]

{- and a list of example expressions -}

badExamples :: [Expr]
badExamples = [
        EA "addOne" [EV "a"],
        EA "missing" [],
        EV "fish",
        EA "infinite" []
    ]

{- such that every element of -}

testBadExamples :: [Val]
testBadExamples = map (eval badProg []) badExamples

{- goes wrong in some way. -}

{-
Try to find as many different kinds of thing which can possibly go
wrong. Exercise as much wickedness as you can muster!
-}
