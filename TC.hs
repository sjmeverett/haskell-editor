module TC where

import Data.Maybe
import Data.Traversable
import Data.Foldable
import Control.Applicative
import Control.Monad

import FOUL
import Prac6Sol
import Prac7Sol

type DName = String
type PName = String
data TypeF t
  = TC DName [t]
  | TP PName
  deriving Show

instance Traversable TypeF where
  traverse h (TC d ts) = TC d <$> traverse h ts
instance Foldable TypeF where
  foldMap = foldMapDefault
instance Functor TypeF where
  fmap = fmapDefault
instance HalfZippable TypeF where
  halfZip (TC d1 ts1) (TC d2 ts2) | d1 == d2 = TC d1 <$> halfZip ts1 ts2
  halfZip (TP p1) (TP p2) | p1 == p2 = pure (TP p1)
  halfZip _ _ = Nothing
instance HalfZippable [] where
  halfZip [] [] = pure []
  halfZip (a : as) (b : bs) = (:) (a, b) <$> halfZip as bs
  halfZip _ _ = Nothing

type Type = Tr TypeF Int
type FType = ([Type], Type)  -- a list of input types, output type

tp :: String -> Type
tp x = Co (TP x)

tc :: DName -> [Type] -> Type
tc d ts = Co (TC d ts)

myData :: [(CName, FType)]
myData
  = [  ("True",  ([],                            tc "Bool" []))
    ,  ("False", ([],                            tc "Bool" []))
    ,  ("Nil",   ([],                            tc "List" [tp "x"]))
    ,  ("Cons",  ([tp "x", tc "List" [tp "x"]],  tc "List" [tp "x"]))
    ]

data TCF x
  = TCAbort
  | TCGet Int (Maybe Type -> x)
  | TCSet Int Type x
  | TCNext (Int -> x)
  | TCCon CName (FType -> x)
  | TCFun FName (FType -> x)

type TCM = Tr TCF

instance Functor TCF where
  fmap h TCAbort = TCAbort
  fmap h (TCGet i k) = TCGet i (h . k)
  fmap h (TCSet i t k) = TCSet i t (h k)
  fmap h (TCNext k) = TCNext (h . k)
  fmap h (TCCon c k) = TCCon c (h . k)
  fmap h (TCFun f k) = TCFun f (h . k)

tcAbort :: TCM a
tcAbort = Co TCAbort
tcGet :: Int -> TCM (Maybe Type)
tcGet i = Co (TCGet i return)
tcSet :: Int -> Type -> TCM ()
tcSet i t = Co (TCSet i t (return ()))
tcNext :: TCM Int
tcNext = Co (TCNext return)
tcCon :: CName -> TCM ([Type], Type)
tcCon c = Co (TCCon c return)
tcFun :: CName -> TCM ([Type], Type)
tcFun f = Co (TCFun f return)

tcRun :: [(CName, ([Type], Type))] ->
         [(Int, Type)] -> Int -> TCM x -> Maybe ([(Int, Type)], x)
tcRun cfs sub nxt (Va x) = return (sub, x)
tcRun cfs sub nxt (Co TCAbort) = Nothing
tcRun cfs sub nxt (Co (TCGet i k)) = tcRun cfs sub nxt (k (lookup i sub))
tcRun cfs sub nxt (Co (TCSet i t k)) = tcRun cfs ((i, t) : sub) nxt k
tcRun cfs sub nxt (Co (TCNext k)) = tcRun cfs sub (nxt + 1) (k nxt)
tcRun cfs sub nxt (Co (TCCon c k)) = do
  t <- lookup c cfs
  tcRun cfs sub nxt (k t)
tcRun cfs sub nxt (Co (TCFun c k)) = do
  t <- lookup c cfs
  tcRun cfs sub nxt (k t)

tcUMonad :: UMonad TypeF Int x -> TCM x
tcUMonad (Va x) = Va x
tcUMonad (Co Abort) = tcAbort
tcUMonad (Co (Get v k)) = do
  t <- tcGet v
  tcUMonad (k t)
tcUMonad (Co (Set v t k)) = do
  tcSet v t
  tcUMonad k

may :: Maybe x -> TCM x
may Nothing = tcAbort
may (Just x) = return x

tcUnify :: Type -> Type -> TCM ()
tcUnify s t = tcUMonad (unify (s, t))

freshen :: [(String, Int)] -> Type -> TCM ([(String, Int)], Type)
freshen already (Va i) = return (already, Va i)
freshen already (Co (TP x)) = case lookup x already of
  Just i -> return (already, Va i)
  Nothing -> do
    i <- tcNext
    return ((x, i) : already, Va i)
freshen already (Co (TC d ts)) = do
  (already, ts) <- freshens already ts
  return (already, Co (TC d ts))

  -- this could be made tidier with a little more cunning
freshens :: [(String, Int)] -> [Type] -> TCM ([(String, Int)], [Type])
freshens already [] = return (already, [])
freshens already (t : ts) = do
  (already, t) <- freshen already t
  (already, ts) <- freshens already ts
  return (already, t : ts)

freshFType :: FType -> TCM FType
freshFType (ss, t) = do
  (xis, ss) <- freshens [] ss
  (_, t) <- freshen xis t
  return (ss, t)

conTy :: FName -> TCM FType
conTy c = do
  t <- tcCon c
  freshFType t

funTy :: FName -> TCM FType
funTy f = do
  t <- tcFun f
  freshFType t

checkPat :: (Type, Pat) -> TCM [(VName, Type)]
checkPat (t, PV x) = return [(x, t)]
checkPat (t, PC c ps) = do
  (ss, t') <- conTy c
  tcUnify t' t
  pss <- may (halfZip ss ps)
  vtss <- traverse checkPat pss
  combine vtss

combine :: [[(VName, Type)]] -> TCM [(VName, Type)]
combine [] = return []
combine (vts : more) = do
  vts' <- combine more
  case [v | (v, _) <- vts, isJust (lookup v vts')] of
    [] -> return (vts ++ vts')
    _  -> tcAbort

checkExp :: [(VName, Type)] -> (Type, Expr) -> TCM ()
checkExp vts (t, EV v) = do
  s <- may (lookup v vts)
  tcUnify s t
checkExp vts (t, EC c es) = do
  (ss, t') <- conTy c
  tcUnify t' t
  ess <- may (halfZip ss es)
  traverse (checkExp vts) ess
  return ()
checkExp vts (t, EA f es) = do
  (ss, t') <- funTy f
  tcUnify t' t
  ess <- may (halfZip ss es)
  traverse (checkExp vts) ess
  return ()
