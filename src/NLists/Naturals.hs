{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}

module NLists.Naturals where

import GHC.TypeLits hiding ((+)(), (*)(), (-)(), (^)())
import qualified GHC.TypeLits as N

-- Peano Numbers for type-level computation.

data Peano = Zero | Succ Peano
  deriving (Eq, Show)

type N0 = Zero
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4

infixr 8  ^
infixl 7  *
infixl 6  +

type family (x :: Peano) + (y :: Peano) :: Peano where
  'Zero + y = y
  ('Succ x) + y = 'Succ (x + y)

type family (x :: Peano) - (y :: Peano) :: Peano where
  x - 'Zero = x
  ('Succ x) - ('Succ y) = x - y

type family (x :: Peano) * (y :: Peano) :: Peano where
  x * 'Zero = 'Zero
  x * ('Succ y) = x + x * y 

type family (x :: Peano) ^ (y :: Peano) :: Peano where
  x ^ 'Zero = 'Succ 'Zero
  x ^ ('Succ y) = x * x ^ y

type family (x :: Peano) < (y :: Peano) where
   x < 'Zero = 'False
   'Zero < y = 'True
   'Succ x < 'Succ y = x < y

type family Min (x :: Peano) (y :: Peano) :: Peano where
  Min x 'Zero = 'Zero
  Min 'Zero y = 'Zero
  Min ('Succ x) ('Succ y) = 'Succ (Min x y)

type family Max (x :: Peano) (y :: Peano) :: Peano where
  Max x 'Zero = x
  Max 'Zero y = y
  Max ('Succ x) ('Succ y) = 'Succ (Max x y)

type family FromPeano (n :: Peano) :: Nat where
  FromPeano 'Zero = 0
  FromPeano ('Succ n) = 1 N.+ (FromPeano n)

type family ToPeano (n :: Nat) :: Peano where
  ToPeano 0 = 'Zero
  ToPeano n = 'Succ (ToPeano (n N.- 1))
