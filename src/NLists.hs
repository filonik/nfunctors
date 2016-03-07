{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}

module NLists where

import Control.Applicative
import Data.Foldable

import NLists.Naturals
import NLists.NFunctors

-- Lists

type family List (n :: Peano) a where 
  List Zero a = a
  List (Succ n) a = [List n a]

data NList (n :: Peano) a where
  ZList :: a -> NList Zero a
  SList :: [NList n a] -> NList (Succ n) a


class Convertable t (n :: Peano) where
  fromList' :: List n a -> t n a
  toList' :: t n a -> List n a

instance Convertable NList Zero where
  fromList' x = ZList x
  toList' (ZList x) = x

instance (Convertable NList n) => Convertable NList (Succ n) where
  fromList' xs = SList (map fromList' xs)
  toList' (SList xs) = map toList' xs

nlift :: (Convertable t n, Convertable t m) => (List n a -> List m b) -> t n a -> t m b
nlift f = fromList' . f . toList'


instance (Convertable t n, Show (List n a)) => Show (t n a) where
  show = show . toList'


instance Functor (NList n) where
  fmap f (ZList x) = ZList (f x)
  fmap f (SList xs) = SList ((fmap . fmap) f xs)

instance Applicative (NList Zero) where
  pure = ZList
  (ZList f) <*> (ZList x) = ZList (f x)

instance (Applicative (NList n)) => Applicative (NList (Succ n)) where
  pure x = SList [pure x]
  (SList fs) <*> (SList xs) = SList (zipWith (<*>) fs xs)

instance Foldable (NList n) where
  foldMap f (ZList x) = f x 
  foldMap f (SList xs) = (foldMap . foldMap) f xs


instance (Num a, Applicative (NList n)) => Num (NList n a) where
  negate = fmap negate
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  fromInteger = pure . fromInteger
  abs = fmap abs
  signum = fmap signum

instance (o ~ (Max n m)) => NApplicative NList n m o where
  pure' = ZList
  (ZList f) <***> (ZList x) = ZList (f x)
  (ZList f) <***> (SList xs) = SList (map ((ZList f)<***>) xs)
  (SList fs) <***> (ZList x) = SList (map (<***>(ZList x)) fs)
  (SList fs) <***> (SList xs) = SList (zipWith (<***>) fs xs)


-- TODO: Generalize without causing overlapping instances...

instance NFunctor NList (Zero) (Zero) where
  pmap' = id
  zmap' = id
  smap' = id

instance (NFunctor NList m m) => NFunctor NList (Succ m) (Succ m) where
  pmap' = id
  zmap' = id
  smap' = id

instance (NFunctor NList (Zero) n) => NFunctor NList (Zero) (Succ n) where
  pmap' f (SList xs) = SList ((fmap . pmap') f xs)
  zmap' f (SList xs) = SList ((fmap . zmap') f xs)
  smap' f (SList xs) = SList ((fmap . smap') f xs)

instance (NFunctor NList (Succ (Zero)) (Succ n)) => NFunctor NList (Succ (Zero)) (Succ (Succ n)) where
  pmap' f (SList xs) = SList ((fmap . pmap') f xs)
  zmap' f (SList xs) = SList ((fmap . zmap') f xs)
  smap' f (SList xs) = SList ((fmap . smap') f xs)

instance (NFunctor NList (Succ (Succ (Zero))) (Succ (Succ n))) => NFunctor NList (Succ (Succ (Zero))) (Succ (Succ (Succ n))) where
  pmap' f (SList xs) = SList ((fmap . pmap') f xs)
  zmap' f (SList xs) = SList ((fmap . zmap') f xs)
  smap' f (SList xs) = SList ((fmap . smap') f xs)
