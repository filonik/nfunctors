{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}

module NLists.NFunctors where

import NLists.Naturals

-- NFunctors, NApplicative - Functors and Applicatives that keep track of how many times they were applied (i.e. raised to power N).

{-
-- In General:
-- pmap: Removes a level of nesting.
-- zmap: Constant nesting.
-- smap: Appends a level of nesting.
-}

class NFunctor t (m :: Peano) (n :: Peano) where
  pmap' :: (t (Succ m) a -> t m b) -> t (Succ n) a -> t n b
  zmap' :: (t m a -> t m b) -> t n a -> t n b
  smap' :: (t m a -> t (Succ m) b) -> t n a -> t (Succ n) b


infixl 4 <***>

class NApplicative t (m :: Peano) (n :: Peano) (o :: Peano) | m n -> o where
  pure' :: a -> t Zero a
  (<***>) :: t m (a -> b) -> t n a -> t o b


-- WIP: The generic implementations still require type annotations to work, therefore manual expansions of the above:

class NFunctor0 t (n :: Peano) where
  pmap0' :: (t (Succ (Zero)) a -> t (Zero) b) -> t (Succ n) a -> t n b
  zmap0' :: (t (Zero) a -> t (Zero) b) -> t n a -> t n b
  smap0' :: (t (Zero) a -> t (Succ (Zero)) b) -> t n a -> t (Succ n) b

class NFunctor1 t (n :: Peano) where
  pmap1' :: (t (Succ (Succ (Zero))) a -> t (Succ (Zero)) b) -> t (Succ (Succ n)) a -> t (Succ n) b
  zmap1' :: (t (Succ (Zero)) a -> t (Succ (Zero)) b) -> t (Succ n) a -> t (Succ n) b
  smap1' :: (t (Succ (Zero)) a -> t (Succ (Succ (Zero))) b) -> t (Succ n) a -> t (Succ (Succ n)) b

class NFunctor2 t (n :: Peano) where
  pmap2' :: (t (Succ (Succ (Succ (Zero)))) a -> t (Succ (Succ (Zero))) b) -> t (Succ (Succ (Succ n))) a -> t (Succ (Succ n)) b
  zmap2' :: (t (Succ (Succ (Zero))) a -> t (Succ (Succ (Zero))) b) -> t (Succ (Succ n)) a -> t (Succ (Succ n)) b
  smap2' :: (t (Succ (Succ (Zero))) a -> t (Succ (Succ (Succ (Zero)))) b) -> t (Succ (Succ n)) a -> t (Succ (Succ (Succ n))) b
