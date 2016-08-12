{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Data.Nested.Functor where

import Data.Generic.Functor
import Data.Nested.Naturals

data MkNested (f :: * -> *) (n :: Peano) a where
  Flat :: a -> MkNested f (Zero) a
  Nest :: MkNested f n (f a) -> MkNested f (Succ n) a

type family UnNestF x where
  UnNestF (MkNested f (Zero) a) = a
  UnNestF (MkNested f (Succ n) a) = MkNested f n (f a)

unNest :: MkNested f n a -> UnNestF (MkNested f n a)
unNest (Flat x) = x
unNest (Nest x) = x

type family NestedF (f :: * -> *) (n :: Peano) a where 
  NestedF f Zero a = a
  NestedF f (Succ n) a = NestedF f n (f a)

class IsNested f (n :: Peano) where
  fromNested :: MkNested f n a -> NestedF f n a
  toNested :: NestedF f n a -> MkNested f n a

instance (Functor f) => IsNested f (Zero) where
  fromNested = unNest
  toNested = Flat

instance (Functor f, IsNested f n) => IsNested f (Succ n) where
  fromNested = fromNested . unNest
  toNested = Nest . toNested

instance (IsNested f n, Show (NestedF f n a)) => Show (MkNested f n a) where
  show = show . fromNested

instance (Functor f) => Functor (MkNested f (Zero)) where
  fmap f = Flat . f . unNest

instance (Functor f, Functor (MkNested f n)) => Functor (MkNested f (Succ n)) where
  fmap f = Nest . ((fmap . fmap) f) . unNest

instance (Applicative f) => Applicative (MkNested f (Zero)) where
  pure = Flat
  Flat f <*> Flat x = Flat (f x)

instance (Applicative f, Applicative (MkNested f n)) => Applicative (MkNested f (Succ n)) where
  pure = Nest . pure . pure
  Nest f <*> Nest x = Nest ((<*>) <$> f <*> x)


-- TODO: Generalize / Reduce Copy Pasta

npush :: (MkNested f m (f a) -> MkNested f n (f b)) -> MkNested f (Succ m) a -> MkNested f (Succ n) b
npush f = Nest . f . unNest

npop :: (MkNested f (Succ m) a -> MkNested f (Succ n) b) -> MkNested f m (f a) -> MkNested f n (f b)
npop f = unNest . f . Nest

nlift :: (IsNested f n, IsNested f m) => (NestedF f n a -> NestedF f m b) -> MkNested f n a -> MkNested f m b
nlift f = toNested . f . fromNested

nunlift :: (IsNested f n, IsNested f m) => (MkNested f n a -> MkNested f m b) -> NestedF f n a -> NestedF f m b
nunlift f = fromNested . f . toNested


class NAdd f (n :: Peano) where
  padd :: (f (Succ n) a -> f n b) -> f (Succ (Succ n)) a -> f (Succ n) b
  zadd :: (f n a -> f n b) -> f (Succ n) a -> f (Succ n) b
  sadd :: (f n a -> f (Succ n) b) -> f (Succ n) a -> f (Succ (Succ n)) b

instance (Functor f) => NAdd (MkNested f) (Zero) where
  padd = (nlift . fmap . nunlift)
  zadd = (nlift . fmap . nunlift)
  sadd = (nlift . fmap . nunlift)

instance (Functor f, NAdd (MkNested f) n) => NAdd (MkNested f) (Succ n) where
  padd f = (npush (padd (npop f)))
  zadd f = (npush (zadd (npop f)))
  sadd f = (npush (sadd (npop f)))


class NFunctor f (m :: Peano) (n :: Peano) where
  pmap :: (f (Succ m) a -> f m b) -> f (Succ n) a -> f n b
  zmap :: (f m a -> f m b) -> f n a -> f n b
  smap :: (f m a -> f (Succ m) b) -> f n a -> f (Succ n) b

instance (Functor f) => NFunctor (MkNested f) (Zero) (Zero) where
  pmap = id
  zmap = id
  smap = id

instance (Functor f, NFunctor (MkNested f) m m) => NFunctor (MkNested f) (Succ m) (Succ m) where
  pmap = id
  zmap = id
  smap = id

instance (Functor f, NAdd (MkNested f) n, NFunctor (MkNested f) (Zero) n) => NFunctor (MkNested f) (Zero) (Succ n) where
  pmap = padd . pmap
  zmap = zadd . zmap
  smap = sadd . smap

instance (Functor f, NAdd (MkNested f) n, NFunctor (MkNested f) (Succ (Zero)) (Succ n)) => NFunctor (MkNested f) (Succ (Zero)) (Succ (Succ n)) where
  pmap = padd . pmap
  zmap = zadd . zmap
  smap = sadd . smap


instance (NFunctor f (Zero) n) => GenericFunctor (f (Zero)) (f (Zero)) (f n) (f n) where nmap = zmap
instance (NFunctor f (Zero) n) => GenericFunctor (f (Zero)) (f (Succ (Zero))) (f n) (f (Succ n)) where nmap = smap
instance (NFunctor f (Zero) n) => GenericFunctor (f (Succ (Zero))) (f (Zero)) (f (Succ n)) (f n) where nmap = pmap

instance (NFunctor f (Succ (Zero)) n) => GenericFunctor (f (Succ (Zero))) (f (Succ (Zero))) (f n) (f n) where nmap = zmap
instance (NFunctor f (Succ (Zero)) n) => GenericFunctor (f (Succ (Zero))) (f (Succ (Succ (Zero)))) (f n) (f (Succ n)) where nmap = smap
instance (NFunctor f (Succ (Zero)) n) => GenericFunctor (f (Succ (Succ (Zero)))) (f (Succ (Zero))) (f (Succ n)) (f n) where nmap = pmap

instance (NFunctor f (Succ (Succ (Zero))) n) => GenericFunctor (f (Succ (Succ (Zero)))) (f (Succ (Succ (Zero)))) (f n) (f n) where nmap = zmap
instance (NFunctor f (Succ (Succ (Zero))) n) => GenericFunctor (f (Succ (Succ (Zero)))) (f (Succ (Succ (Succ (Zero))))) (f n) (f (Succ n)) where nmap = smap
instance (NFunctor f (Succ (Succ (Zero))) n) => GenericFunctor (f (Succ (Succ (Succ (Zero))))) (f (Succ (Succ (Zero)))) (f (Succ n)) (f n) where nmap = pmap
