{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}

module NLists.NFunctors where

import NLists.GenericPrelude
import NLists.Naturals

-- NFunctors, NApplicative - Functors and Applicatives that keep track of how many times they were applied (i.e. raised to power N).

{-
-- In General:
-- pmap: Removes a level of nesting.
-- zmap: Constant nesting.
-- smap: Appends a level of nesting.
-}

infixl 4 <***> -- Same as <*>, <**> in Applicative


class NFunctor t (m :: Peano) (n :: Peano) where
  pmap' :: (t (Succ m) a -> t m b) -> t (Succ n) a -> t n b
  zmap' :: (t m a -> t m b) -> t n a -> t n b
  smap' :: (t m a -> t (Succ m) b) -> t n a -> t (Succ n) b


class NApplicative t (m :: Peano) (n :: Peano) (o :: Peano) | m n -> o where
  pure' :: a -> t Zero a
  (<***>) :: t m (a -> b) -> t n a -> t o b


-- TODO: Generalize without causing overlapping instances...

instance (NFunctor t (Zero) n) => GenericFunctor (t (Zero)) (t (Zero)) (t n) (t n) where fmap' = zmap'
instance (NFunctor t (Zero) n) => GenericFunctor (t (Zero)) (t (Succ (Zero))) (t n) (t (Succ n)) where fmap' = smap'
instance (NFunctor t (Zero) n) => GenericFunctor (t (Succ (Zero))) (t (Zero)) (t (Succ n)) (t n) where fmap' = pmap'

instance (NFunctor t (Succ (Zero)) n) => GenericFunctor (t (Succ (Zero))) (t (Succ (Zero))) (t n) (t n) where fmap' = zmap'
instance (NFunctor t (Succ (Zero)) n) => GenericFunctor (t (Succ (Zero))) (t (Succ (Succ (Zero)))) (t n) (t (Succ n)) where fmap' = smap'
instance (NFunctor t (Succ (Zero)) n) => GenericFunctor (t (Succ (Succ (Zero)))) (t (Succ (Zero))) (t (Succ n)) (t n) where fmap' = pmap'

instance (NFunctor t (Succ (Succ (Zero))) n) => GenericFunctor (t (Succ (Succ (Zero)))) (t (Succ (Succ (Zero)))) (t n) (t n) where fmap' = zmap'
instance (NFunctor t (Succ (Succ (Zero))) n) => GenericFunctor (t (Succ (Succ (Zero)))) (t (Succ (Succ (Succ (Zero))))) (t n) (t (Succ n)) where fmap' = smap'
instance (NFunctor t (Succ (Succ (Zero))) n) => GenericFunctor (t (Succ (Succ (Succ (Zero))))) (t (Succ (Succ (Zero)))) (t (Succ n)) (t n) where fmap' = pmap'
