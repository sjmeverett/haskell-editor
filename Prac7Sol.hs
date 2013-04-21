{-# LANGUAGE TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances #-}

module Prac7Sol where

{- Identify yourself. -------------------------------------}

  {- Name:  Conor                          -}

{----------------------------------------------------------}

import Prac6Sol

import Data.Maybe
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Monad

{-----------------------}
{- Stuff from lectures -}
{-----------------------}

{- Foldable and Traversable instances for the functor kit -}

instance Traversable (K a) where
  traverse h (K a) = pure (K a)
instance Foldable (K a) where
  foldMap h (K a) = mempty

instance Traversable I where
  traverse h (I x) = pure I <*> h x
instance Foldable I where
  foldMap h (I x) = h x

instance (Traversable f, Traversable g) => Traversable (f :+: g) where
  traverse h (L fx) = pure L <*> traverse h fx
  traverse h (R gx) = pure R <*> traverse h gx
instance (Foldable f, Foldable g) => Foldable (f :+: g) where
  foldMap h (L fx) = foldMap h fx
  foldMap h (R gx) = foldMap h gx

instance (Traversable f, Traversable g) => Traversable (f :*: g) where
  traverse h (fx :&: gx) = pure (:&:) <*> traverse h fx <*> traverse h gx
instance (Foldable f, Foldable g) => Foldable (f :*: g) where
  foldMap h (fx :&: gx) = mappend (foldMap h fx) (foldMap h gx)

{- HalfZippable and instances -}

class HalfZippable f where
  halfZip :: f a -> f b -> Maybe (f (a, b))

instance Eq k => HalfZippable (K k) where
  halfZip (K k) (K l) = do
    guard (k == l)
    return (K k)

instance HalfZippable I where
  halfZip (I a) (I b) = return (I (a, b))

instance (HalfZippable f, HalfZippable g) => HalfZippable (f :+: g) where
  halfZip (L fa) (L fb)  = pure L <*> halfZip fa fb
  halfZip (R ga) (R gb)  = pure R <*> halfZip ga gb
  halfZip _      _       = Nothing

instance (HalfZippable f, HalfZippable g) => HalfZippable (f :*: g) where
  halfZip (fa :&: ga) (fb :&: gb) = pure (:&:) <*> halfZip fa fb <*> halfZip ga gb

{- generic equality -}

instance (Traversable f, HalfZippable f) => Eq (Data f) where
  a == b = isJust (eqTest (a, b)) where
    eqTest (Node fdf, Node fdf') = do
      fpair <- halfZip fdf fdf'
      traverse eqTest fpair
      return ()

{- The Free Monad, seen as general purpose trees. -}

data Tr f v
  = Va v                 -- values, or variables
  | Co (f (Tr f v))    -- command, or constructor

instance Functor f => Monad (Tr f) where
  return v = Va v
  Va x   >>= g = g x
  Co ftf >>= g = Co (fmap (>>= g) ftf)

instance Functor f => Applicative (Tr f) where
  pure = return
  tf <*> ta = do
    f <- tf
    a <- ta
    return (f a)

instance Functor f => Functor (Tr f) where
  fmap f ta = pure f <*> ta

{- Building a Monad for solving unification problems. -}

{- The following type explains what *one* node of a command-response
   tree is, when you're solving a unification problem. -}

data UCommand f v x
  = Abort
  | Get v (Maybe (Tr f v) -> x)
  | Set v (Tr f v) x

{- Q1 -}

{- Complete the Functor instance -}

instance Functor (UCommand f v) where
  fmap h Abort        = Abort
  fmap h (Get v k)    = Get v (h . k)
  fmap h (Set v t k)  = Set v t (h k)

type UMonad f v = Tr (UCommand f v)

{- Q2 -}

{- Define the individual commands -}

abort :: UMonad f v x
abort = Co Abort

get :: v -> UMonad f v (Maybe (Tr f v))
get v = Co (Get v return)

set :: v -> Tr f v -> UMonad f v ()
set v t = Co (Set v t (return ()))


{- Q3 -}

type Subst f v = [(v, Tr f v)]

{- Define a function to interpret command-response trees, given an initial substitution,
   returning (if successful) a value and the final substitution. -}

runUCommands :: Eq v => Subst f v -> UMonad f v x -> Maybe (Subst f v, x)
runUCommands sub (Va x)            = return (sub, x)
runUCommands sub (Co Abort)        = fail "aborted"
runUCommands sub (Co (Get v k))    = runUCommands sub (k (lookup v sub))
runUCommands sub (Co (Set v t k))  = runUCommands ((v, t) : sub) k


{- Q4 -}

{- Define a function to check if a term depends on a variable. It should abort if there's
   a dependency, and succeed with a trivial value otherwise. -}

noDependency :: (Eq v, Traversable f) => v -> Tr f v -> UMonad f v ()
noDependency v (Va u)
  | v == u = abort
  | otherwise = do
    mt <- get u
    case mt of
      Nothing -> return ()
      Just s -> noDependency v s
noDependency v (Co ts) = () <$ traverse (noDependency v) ts


{- Note that if t depends on x and x is defined by the substitution to be a term
   depending on v, then t depends on v. That is, you need to look up variables to
   check dependency thoroughly. -}

{- Q5 -}

{- Implement unification! When you encounter variables, check dependency before
   extending the substitution. -}

headNormal :: Tr f v -> UMonad f v (Tr f v)
headNormal (Va v) = do
   mv <- get v
   case mv of
     Nothing -> return (Va v)
     Just s -> headNormal s
headNormal t = return t

unify :: (Eq v, Traversable f, HalfZippable f) =>
         (Tr f v, Tr f v) -> UMonad f v ()
unify (s, t) = do
  s <- headNormal s
  t <- headNormal t
  case (s, t) of
    (Va u, Va v) | u == v -> return ()
    (Va v, t) -> do
      noDependency v t
      set v t
    (t, Va v) -> do
      noDependency v t
      set v t
    (Co ss, Co ts) -> case halfZip ss ts of
      Nothing -> abort
      Just ps -> () <$ traverse unify ps


type TyF = K () :+: (I :*: I)

base :: Tr TyF String
base = Co (L (K ()))

arr :: Tr TyF String -> Tr TyF String -> Tr TyF String
arr s t = Co (R (I s :&: I t))

instance (Show v, Show (f (Tr f v))) => Show (Tr f v) where
  show (Va v) = "(Va " ++ show v ++ ")"
  show (Co t) = "(Co " ++ show t ++ ")"
