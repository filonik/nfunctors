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


infixl 4 <<$>> -- Same as <$> in Functor
infixl 4 <<$$>> -- Same as <$> in Functor
infixl 4 <<*>> -- Same as <*> in Applicative
infixl 4 <<**>> -- Same as <*> in Applicative

-- TODO: Unification (Functor => GenericFunctor / Applicative => GenericApplicative)
{-
class GenericFunctor p q r s | p q r -> s, p r s -> q where
  fmap' :: (p a -> q b) -> r a -> s b
  (<<$>>) :: (p a -> q b) -> r a -> s b
  (<<$$>>) :: r a -> (p a -> q b) -> s b
  (<<$>>) = fmap'
  (<<$$>>) = flip (<<$>>)

instance (Functor t) => GenericFunctor Identity Identity t t where
  fmap' f = fmap (runIdentity . f . Identity)

class GenericApplicative p q r s | q r s -> p where
  pure' :: a -> p a
  (<<*>>) :: q (a -> b) -> r a -> s b
  (<<**>>) :: r a -> q (a -> b) -> s b
  (<<**>>) = flip (<<*>>)
  
instance (Applicative t) => GenericApplicative t t t t where
  pure' = pure
  (<<*>>) = (<*>)
-}

class GenericFunctor s t (m0 :: Peano) (m1 :: Peano) (n0 :: Peano) (n1 :: Peano) | s t m0 m1 n0 -> n1, s t m0 n0 n1 -> m1 where
  fmap' :: (s m0 a -> s m1 b) -> t n0 a -> t n1 b
  (<<$>>) :: (s m0 a -> s m1 b) -> t n0 a -> t n1 b
  (<<$$>>) :: t n0 a -> (s m0 a -> s m1 b) -> t n1 b
  (<<$>>) = fmap'
  (<<$$>>) = flip (<<$>>)

class NApplicative t (m :: Peano) (n :: Peano) (o :: Peano) | t m n -> o  where
  pure' :: a -> t Zero a
  (<<*>>) :: t m (a -> b) -> t n a -> t o b
  (<<**>>) :: t n a -> t m (a -> b) -> t o b
  (<<**>>) = flip (<<*>>)

class NFunctor t (m :: Peano) (n :: Peano) where
  pmap' :: (t (Succ m) a -> t m b) -> t (Succ n) a -> t n b
  zmap' :: (t m a -> t m b) -> t n a -> t n b
  smap' :: (t m a -> t (Succ m) b) -> t n a -> t (Succ n) b


-- TODO: Generalize without causing overlapping instances...

instance (NFunctor t (Zero) n) => GenericFunctor t t (Zero) (Zero) n n where fmap' = zmap'
instance (NFunctor t (Zero) n) => GenericFunctor t t (Zero) (Succ (Zero)) n (Succ n) where fmap' = smap'
instance (NFunctor t (Zero) n) => GenericFunctor t t (Succ (Zero)) (Zero) (Succ n) n where fmap' = pmap'

instance (NFunctor t (Succ (Zero)) n) => GenericFunctor t t (Succ (Zero)) (Succ (Zero)) n n where fmap' = zmap'
instance (NFunctor t (Succ (Zero)) n) => GenericFunctor t t (Succ (Zero)) (Succ (Succ (Zero))) n (Succ n) where fmap' = smap'
instance (NFunctor t (Succ (Zero)) n) => GenericFunctor t t (Succ (Succ (Zero))) (Succ (Zero)) (Succ n) n where fmap' = pmap'

instance (NFunctor t (Succ (Succ (Zero))) n) => GenericFunctor t t (Succ (Succ (Zero))) (Succ (Succ (Zero))) n n where fmap' = zmap'
instance (NFunctor t (Succ (Succ (Zero))) n) => GenericFunctor t t (Succ (Succ (Zero))) (Succ (Succ (Succ (Zero)))) n (Succ n) where fmap' = smap'
instance (NFunctor t (Succ (Succ (Zero))) n) => GenericFunctor t t (Succ (Succ (Succ (Zero)))) (Succ (Succ (Zero))) (Succ n) n where fmap' = pmap'
