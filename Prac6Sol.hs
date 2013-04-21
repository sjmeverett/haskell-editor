{-# LANGUAGE TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances #-}

module Prac6Sol where

{- Identify yourself. -------------------------------------}

  {- Name: Conor McBride                       -}

{----------------------------------------------------------}


{- The Functor Kit -}

data K a        x   = K a                  -- for labelled leaves
  deriving (Show, Eq)
data I          x   = I x                  -- for substructure places
  deriving (Show, Eq)
data (f :+: g)  x   = L (f x) | R (g x)    -- offering a choice of things
  deriving (Show, Eq)
data (f :*: g)  x   = f x :&: g x          -- making a pair of things
  deriving (Show, Eq)
infixr 4 :&:


{-Q1-begin---------------------------------------------------}
{- Complete the Functor instances. -}

instance Functor (K a) where
  fmap m (K a) = K a

instance Functor I where
  fmap m (I x) = I (m x)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap m (L fx) = L (fmap m fx)
  fmap m (R gx) = R (fmap m gx)

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap m (fx :&: gx) = fmap m fx :&: fmap m gx
{-Q1-end-----------------------------------------------------}

{- Making a datatype from a functor. -}

data Data f = Node (f (Data f))

instance Show (f (Data f)) => Show (Data f) where
  show (Node x) = "(Node (" ++ show x ++ "))"

{- The generic fold operator, or "catamorphism" -}

dataFold :: Functor f => (f t -> t) -> Data f -> t
dataFold phi (Node fdf) = phi (fmap (dataFold phi) fdf)

{- The generic recursion operator, or "paramorphism" -}

dataRec :: Functor f => (f (Data f, t) -> t) -> Data f -> t
dataRec phi (Node fdf) = phi (fmap (\ df -> (df, dataRec phi df)) fdf)

{- The difference between the two is that
      dataFold just gives you the value computed recursively for
        each child, while
      dataRec gives you the pair of the child itself and the value
        computed recursively for it.
-}


{- Example datatype: "Hutton's Razor" expressions. -}

data HuttonExp
  = Val Int
  | Add HuttonExp HuttonExp
  deriving (Show)

{- So
  Val :: Int ->                     HuttonExp
  Add :: HuttonExp -> HuttonExp ->  HuttonExp
-}

{- "Hutton's functor", generating binary trees with numbers at the leaves. -}

type HuttonF
  =    K Int
  :+:  (I :*: I)

{- Data HuttonF gives us HuttonExp in terms of the functor kit. -}
{- We can rebuild the constructors. -}

val :: Int -> Data HuttonF
val i = Node (L (K i))

add :: Data HuttonF -> Data HuttonF -> Data HuttonF
add e1 e2 = Node (R (I e1 :&: I e2))

{-Q2-begin---------------------------------------------------}
{- Write both directions of the translation between the two versions.
   Be sure to give the second in terms of foldData -}

hutToKit :: HuttonExp -> Data HuttonF
hutToKit (Val i)      = val i
hutToKit (Add e1 e2)  = add (hutToKit e1) (hutToKit e2)

kitToHut :: Data HuttonF -> HuttonExp
kitToHut = dataFold expAlgebra where
  expAlgebra :: HuttonF HuttonExp -> HuttonExp
  expAlgebra (L (K i))            = Val i
  expAlgebra (R (I e1 :&: I e2))  = Add e1 e2
{-Q2-end-----------------------------------------------------}


{-Q3-begin---------------------------------------------------}
{- Implement an evaluator for Data HuttonF using dataFold -}

kitEval :: Data HuttonF -> Int
kitEval = dataFold evalAlgebra where
  evalAlgebra :: HuttonF Int -> Int
  evalAlgebra (L (K i))            = i
  evalAlgebra (R (I v1 :&: I v2))  = v1 + v2
{-Q3-end-----------------------------------------------------}


{- Let me give you an example of dataRec in action. -}

allSubExpressions :: Data HuttonF -> [Data HuttonF]
allSubExpressions = dataRec subAlgebra where
  subAlgebra :: HuttonF (Data HuttonF, [Data HuttonF]) -> [Data HuttonF]
  subAlgebra (L _)                            = []
  subAlgebra (R (I (e, es) :&: I (e', es')))  = e : es ++ e' : es'

  --               ^^  ^^
  --   the left child, all of the left child's sub-expressions

  -- try it and see

  -- allSubExpressions (add (add (val 1) (val 2)) (add (val 3) (val 4)))

{- Now your turn. -}


{-Q4-begin---------------------------------------------------}
{- Make your own version of [a]. -}

type ListF a = K () :+: (K a :*: I) -- replaced this right-hand side
   -- with a Functor from the kit which
   -- offers the choice of "nil" or "cons";
   -- treat the head as a label and the tail as a child

{- So your version of [a] should be given by the following... -}

type List a = Data (ListF a)

{- Now define constructors. -}

nil :: List a
nil = Node (L (K ()))

cons :: a -> List a -> List a
cons x xs = Node (R (K x :&: I xs))

{- Define the function to append two lists, using dataFold. -}

append :: List a -> List a -> List a
append xs ys = dataFold appendAlg xs where
  -- appendAlg :: ListF a (List a) -> List a -- for info
  appendAlg (L (K ())) = ys
  appendAlg (R (K x :&: I zs)) = cons x zs
{-Q4-end-----------------------------------------------------}

{-Q5-begin---------------------------------------------------}
{- Insertion Sort -}

{- Define the function to insert an element into a list so that everything
   left of it is smaller than it. You will need dataRec for that. -}

insertIntoList :: Ord a => a -> List a -> List a
insertIntoList a = dataRec insAlg where
  -- insAlg :: ListF a (List a, List a) -> List a
           --           ^^      ^^
           --           ||    what you get if you insert a in the tail
           --         the original tail
  insAlg (L (K ())) = cons a nil
  insAlg (R (K x :&: I (xs, axs)))
    | x < a      = cons x axs
    | otherwise  = cons a (cons x xs)


{- Now, making use of insertIntoList, use dataFold to define
   insertion-sort. -}

insertionSort :: Ord a => List a -> List a
insertionSort = dataFold insSortAlg where
  -- insSortAlg :: ListF a (List a) -> List a -- for info
  insSortAlg (L (K ())) = nil
  insSortAlg (R (K x :&: I ys)) = insertIntoList x ys
{-Q5-end-----------------------------------------------------}


{-Q6-begin---------------------------------------------------}
{- Give a functor capturing the structure of node-labelled binary trees -}

type TreeF a = K () :+: (I :*: (K a :*: I))
type Tree a = Data (TreeF a)

{- So that (Tree a) corresponds to good old... -}

data BTree a
  = BLeaf 
  | BNode (BTree a) a (BTree a)
  deriving Show

{- Define constructors. -}

leaf :: Tree a
leaf = Node (L (K ()))

node :: Tree a -> a -> Tree a -> Tree a
node l a r = Node (R (I l :&: K a :&: I r))

{- Show how to flatten a tree into a list, using dataFold. -}

flatten :: Tree a -> List a
flatten = dataFold flatAlg where
  -- flatAlg :: TreeF a (List a) -> List a -- for info
  flatAlg (L (K ())) = nil
  flatAlg (R (I xs :&: K a :&: I ys)) = append xs (cons a ys)

{- Show how to insert an element into a binary search tree. Use
   either dataFold or dataRec, rather than explicit recursion. -}

insertIntoTree :: Ord a => a -> Tree a -> Tree a
insertIntoTree a = dataRec insAlg where
  insAlg (L (K ())) = node leaf a leaf
  insAlg (R (I (l, al) :&: K b :&: I (r, ar)))
    | a < b     = node al b r
    | otherwise = node l b ar

{- Show how to build a binary search tree from a list, using
   dataFold or dataRec, along with insertIntoTree. -}

makeTree :: Ord a => List a -> Tree a
makeTree = dataFold makeAlg where
  makeAlg (L (K ())) = leaf
  makeAlg (R (K x :&: I t)) = insertIntoTree x t

{- When you're done, you should find that the following works. -}

treeSort :: Ord a => List a -> List a
treeSort = flatten . makeTree
{-Q6-end-----------------------------------------------------}


{- Now let's investigate the combinatorics of these functors. -}
{- We can make a class for types whose elements we can list. -}

class Listable x where
  allElements :: [x]

instance Listable Void where       -- the "zero" type
  allElements = []

instance Listable () where         -- the "one" type
  allElements = [()]

instance Listable Bool where       -- the "two" type
  allElements = [False, True]

data Three = One | Two | Three deriving (Show, Eq)
instance Listable Three where
  allElements = [One, Two, Three]


{-Q7-begin---------------------------------------------------}
{- Define these Listable instances for the functor kit, showing
   how to list all the possible data. -}

{- Hint: list comprehensions. -}

instance Listable a => Listable (K a x) where
  allElements = [K a | a <- allElements]

instance Listable x => Listable (I x) where
  allElements = [I x | x <- allElements]

instance (Listable (f x), Listable (g x)) => Listable ((f :+: g) x) where
  allElements = [L l | l <- allElements] ++ [R r | r <- allElements]

instance (Listable (f x), Listable (g x)) => Listable ((f :*: g) x) where
  allElements = [l :&: r | l <- allElements, r <- allElements]

{-Q7-end-----------------------------------------------------}

{- And with that done, check out the values of... -}

type E = K ()
countsE :: [Int]
countsE =  [  length (allElements :: [E Void])
           ,  length (allElements :: [E ()])
           ,  length (allElements :: [E Bool])
           ,  length (allElements :: [E Three])
           ]

type F = I
countsF :: [Int]
countsF =  [  length (allElements :: [F Void])
           ,  length (allElements :: [F ()])
           ,  length (allElements :: [F Bool])
           ,  length (allElements :: [F Three])
           ]


type G = I :*: I
countsG :: [Int]
countsG =  [  length (allElements :: [G Void])
           ,  length (allElements :: [G ()])
           ,  length (allElements :: [G Bool])
           ,  length (allElements :: [G Three])
           ]

type H = I :*: I :*: I
countsH :: [Int]
countsH =  [  length (allElements :: [H Void])
           ,  length (allElements :: [H ()])
           ,  length (allElements :: [H Bool])
           ,  length (allElements :: [H Three])
           ]

type J = F :+: G
countsJ :: [Int]
countsJ =  [  length (allElements :: [J Void])
           ,  length (allElements :: [J ()])
           ,  length (allElements :: [J Bool])
           ,  length (allElements :: [J Three])
           ]


{- Useful kit ---------------------------------------------------------}

{- One way to define the empty type -}

newtype Void = Avoid {absurd :: forall a. a}
instance Show Void where
  show x = absurd x
instance Eq Void where
  x == y = absurd x

{- A placeholder functor, so the incomplete file typechecks -}

newtype Undefined x = Undefined (Undefined x) -- I'll use this as a placeholder
instance Functor Undefined where
  fmap f (Undefined ux) = Undefined (fmap f ux)
