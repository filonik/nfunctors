{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}

module NLists.NFunctors where

import NLists.Naturals

-- NFunctors - Functors that keep track of how many times they were applied (i.e. raised to power N).

{-
-- In General:
-- pmap: Removes a level of nesting.
-- zmap: Constant nesting.
-- smap: Appends a level of nesting.

class NFunctorM t (n :: Peano) where
  pmapM' :: (t M a -> t (M-1) b) -> t (n+M) a -> t (n+M-1) b
  zmapM' :: (t M a -> t M b) -> t (n+M) a -> t (n+M) b
  smapM' :: (t M a -> t (M+1) b) -> t (n+M) a -> t (n+M+1) b
-}

class NFunctor0 t (n :: Peano) where
  zmap0' :: (t (Zero) a -> t (Zero) b) -> t n a -> t n b
  smap0' :: (t (Zero) a -> t (Succ (Zero)) b) -> t n a -> t (Succ n) b

class NFunctor1 t (n :: Peano) where
  pmap1' :: (t (Succ (Zero)) a -> t (Zero) b) -> t (Succ n) a -> t n b
  zmap1' :: (t (Succ (Zero)) a -> t (Succ (Zero)) b) -> t (Succ n) a -> t (Succ n) b
  smap1' :: (t (Succ (Zero)) a -> t (Succ (Succ (Zero))) b) -> t (Succ n) a -> t (Succ (Succ n)) b

class NFunctor2 t (n :: Peano) where
  pmap2' :: (t (Succ (Succ (Zero))) a -> t (Succ (Zero)) b) -> t (Succ (Succ n)) a -> t (Succ n) b
  zmap2' :: (t (Succ (Succ (Zero))) a -> t (Succ (Succ (Zero))) b) -> t (Succ (Succ n)) a -> t (Succ (Succ n)) b
  smap2' :: (t (Succ (Succ (Zero))) a -> t (Succ (Succ (Succ (Zero)))) b) -> t (Succ (Succ n)) a -> t (Succ (Succ (Succ n))) b

