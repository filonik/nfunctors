{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

module NFunctors where

import Data.Proxy
import Data.Functor.Identity

{-
import GHC.TypeLits

type family NFunctorF f (n :: Nat) a where 
  NFunctorF f 0 a = a
  NFunctorF f n a = f (NFunctorF f (n-1) a)

data MkNFunctor f (n :: Nat) a where
  ZF :: a -> MkNFunctor f 0 a
  SF :: f (MkNFunctor f (n-1) a) -> MkNFunctor f n a

class NConvertable f (n :: Nat) where
  fromNFunctor :: MkNFunctor f n a -> NFunctorF f n a
  toNFunctor :: NFunctorF f n a -> MkNFunctor f n a

instance NConvertable f 0 where
  fromNFunctor (ZF x) = x
  toNFunctor x = ZF x

instance (Functor f, NConvertable f (n-1)) => NConvertable f n where
  fromNFunctor (SF xs) = fmap fromNFunctor xs
  toNFunctor xs = SF (fmap toNFunctor xs)

type NMaybe = MkNFunctor Maybe
type NList = MkNFunctor []
-}

import NLists.Naturals

infixl 4 <<$>> -- Same as <$> in Functor
infixl 4 <<$$>> -- Same as <$> in Functor
infixl 4 <<*>> -- Same as <*> in Applicative
infixl 4 <<**>> -- Same as <*> in Applicative

type family NFunctorF f (n :: Peano) a where 
  NFunctorF f Zero a = a
  NFunctorF f (Succ n) a = f (NFunctorF f n a)

-- Basic Idea:
-- (Functor f) => (Functor (MkNested f n) and (MkNested f) => (NFunctor))
-- (Applicative f) => (Applicative (MkNested f n) and (MkNested f) => (NApplicative))

data MkNested f (n :: Peano) a where
  ZF :: a -> MkNested f Zero a
  SF :: f (MkNested f n a) -> MkNested f (Succ n) a

class IsNested f (n :: Peano) where
  fromNested :: MkNested f n a -> NFunctorF f n a
  toNested :: NFunctorF f n a -> MkNested f n a

instance (Functor f) => IsNested f Zero where
  fromNested (ZF x) = x
  toNested x = ZF x

instance (Functor f, IsNested f n) => IsNested f (Succ n) where
  fromNested (SF x) = fmap fromNested x
  toNested x = SF (fmap toNested x)

instance (IsNested f n, Show (NFunctorF f n a)) => Show (MkNested f n a) where
  show = show . fromNested

nlift :: (IsNested f n, IsNested f m) => (NFunctorF f n a -> NFunctorF f m b) -> MkNested f n a -> MkNested f m b
nlift f = toNested . f . fromNested


instance (Functor f) => Functor (MkNested f n) where
  fmap f (ZF x) = ZF (f x)
  fmap f (SF x) = SF ((fmap . fmap) f x)

instance (Applicative f) => Applicative (MkNested f (Zero)) where
  pure = ZF
  (ZF f) <*> (ZF x) = ZF (f x) 

instance (Applicative f, Applicative (MkNested f n)) => Applicative (MkNested f (Succ n)) where
  pure x = SF ((pure . pure) x)
  (SF f) <*> (SF x) = SF ((<*>) <$> f <*> x)

next :: (Applicative f) => (MkNested f n a) -> MkNested f (Succ n) a
next x = SF (pure x)


class NFunctor f (m0 :: Peano) (m1 :: Peano) (n0 :: Peano) (n1 :: Peano) where
  fmap' :: (f m0 a -> f m1 b) -> f n0 a -> f n1 b
  (<<$>>) :: (f m0 a -> f m1 b) -> f n0 a -> f n1 b
  (<<$$>>) :: f n0 a -> (f m0 a -> f m1 b) -> f n1 b
  (<<$>>) = fmap'
  (<<$$>>) = flip (<<$>>)


class Purifiable f (m :: Peano) (n :: Peano) where
  pure' :: f m a -> f n a

-- Gets a bit messy but takes care of overlap.
-- https://wiki.haskell.org/GHC/AdvancedOverlap

class Purifiable' (guard :: Bool) f (m :: Peano) (n :: Peano) where
  pure'' :: Proxy guard -> f m a -> f n a

instance ((m < n) ~ guard, Purifiable' guard f m n) => Purifiable f m n where
  pure' = pure'' (undefined::Proxy guard)

-- End of mess.

instance (Applicative f) => Purifiable' False (MkNested f) n n where
  pure'' _ = id

instance (Applicative f, Purifiable (MkNested f) (Succ m) n) => Purifiable' True (MkNested f) m n where
  pure'' _ = pure' . next


class (Purifiable f m o, Purifiable f n o, Applicative (f o)) => NApplicative f (m :: Peano) (n :: Peano) (o :: Peano) | m n -> o where
  (<<*>>) :: f m (a -> b) -> f n a -> f o b
  (<<**>>) :: f n a -> f m (a -> b) ->  f o b
  (<<*>>) f x = (pure' f) <*> (pure' x)
  (<<**>>) = flip (<<*>>)

instance (Applicative f, Purifiable (MkNested f) m o, Purifiable (MkNested f) n o, Applicative (MkNested f o), o~(Max m n)) => NApplicative (MkNested f) m n o

{-
instance NApplicative (MkNested f) n n where pure' = id
instance (Applicative f, NApplicative (MkNested f) n n) => NApplicative (MkNFunctor f) n (Succ n) where pure' = next . pure'
instance (Applicative f, NApplicative (MkNested f) n n) => NApplicative (MkNFunctor f) n (Succ (Succ n)) where pure' = next . next . pure'
-}

type NIdentity = MkNested Identity
type NMaybe = MkNested Maybe
type NList = MkNested []

