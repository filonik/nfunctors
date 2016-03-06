{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}

module NLists.Naturals where

import GHC.TypeLits hiding ((+)(), (*)(), (-)(), (^)())
import qualified GHC.TypeLits as N

-- Peano Numbers for type-level computation.

data Peano = Zero | Succ Peano

type N0 = Zero
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4

type family (x :: Peano) + (y :: Peano) where
  'Zero + y = y
  'Succ x + y = 'Succ (x + y)

type family FromPeano (n :: Peano) :: Nat where
  FromPeano Zero = 0
  FromPeano (Succ n) = 1 N.+ (FromPeano n)

type family ToPeano (n :: Nat) :: Peano where
  ToPeano 0 = Zero
  ToPeano n = Succ (ToPeano (n N.- 1))
