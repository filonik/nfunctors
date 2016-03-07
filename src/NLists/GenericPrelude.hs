{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module NLists.GenericPrelude where

import Prelude hiding (not,(&&),(||),(==),(/=),(<=),(>=),(<),(>))
import qualified Prelude as P

import Control.Applicative
import Data.Functor.Identity

-- Same as in Prelude

infixr 3 &&
infixr 2 ||

infix 4 <=
infix 4 >=
infix 4 <
infix 4 >


class GenericBool a where
  not :: a -> a
  (&&) :: a -> a -> a
  (||) :: a -> a -> a
  
instance GenericBool Bool where
  not = P.not
  (&&) = (P.&&)
  (||) = (P.||)

instance (Applicative f, GenericBool a) => GenericBool (f a) where
  not = liftA not
  (&&) = liftA2 (&&)
  (||) = liftA2 (||)

class (GenericBool b) => GenericEq a b where
  (==) :: a -> a -> b
  (/=) :: a -> a -> b
  (==) = not . (/=)
  (/=) = not . (==)

class (GenericBool b) => GenericOrd a b where
  (<=) :: a -> a -> b
  (>=) :: a -> a -> b
  (<) :: a -> a -> b
  (>) :: a -> a -> b
  (<=) = not . (>)
  (>=) = not . (<)
  (>) = not . (<=)
  (<) = not . (>=)

instance (Eq a) => GenericEq a Bool where
  (==) = (P.==)
  
instance (Ord a) => GenericOrd a Bool where
  (<=) = (P.<=)
  (>=) = (P.>=)

instance (Applicative f, Eq a) => GenericEq (f a) (f Bool) where
  (==) = liftA2 (P.==)

instance (Applicative f, Ord a) => GenericOrd (f a) (f Bool) where
  (<=) = liftA2 (P.<=)
  (>=) = liftA2 (P.>=)

instance (Applicative f, Num a) => Num (f a) where
  fromInteger = pure . fromInteger
  (+) = liftA2 (P.+)
  (*) = liftA2 (P.*)
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
