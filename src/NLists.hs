{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}

module NLists where

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

nmap :: (Convertable t n, Convertable t m) => (List n a -> List m b) -> t n a -> t m b
nmap f = fromList' . f . toList'

instance (Convertable NList n, Show (List n a)) => Show (NList n a) where
  show = show . toList'

instance Functor (NList n) where
  fmap f (ZList x) = ZList (f x)
  fmap f (SList xs) = SList (map (fmap f) xs)

instance Applicative (NList Zero) where
  pure = ZList
  (ZList f) <*> (ZList x) = ZList (f x)

instance (Applicative (NList n)) => Applicative (NList (Succ n)) where
  pure xs = SList [pure xs]
  (SList fs) <*> (SList xs) = SList (zipWith (<*>) fs xs)

instance Foldable (NList n) where
  foldMap f (ZList x) = f x 
  foldMap f (SList xs) = (foldMap . foldMap) f xs


instance NFunctor0 NList (Zero) where
  zmap0' f = f
  smap0' f = f

instance (NFunctor0 NList n) => NFunctor0 NList (Succ n) where
  zmap0' f (SList xs) = SList (map (zmap0' f) xs)
  smap0' f (SList xs) = SList (map (smap0' f) xs)

instance NFunctor1 NList (Zero) where
  pmap1' f = f
  zmap1' f = f
  smap1' f = f

instance (NFunctor1 NList n) => NFunctor1 NList (Succ n) where
  pmap1' f (SList xs) = SList (map (pmap1' f) xs)
  zmap1' f (SList xs) = SList (map (zmap1' f) xs)
  smap1' f (SList xs) = SList (map (smap1' f) xs)
