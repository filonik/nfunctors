{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Example where

import Control.Arrow
import Control.Applicative
import Data.Function
import Data.List

import NLists.Naturals
import NLists.NFunctors
import NLists


-- Example: SQL-like embedded DSL
-- Problem: https://byorgey.wordpress.com/2007/08/16/mapping-over-a-nested-functor/

instance (Num b) => Num (a -> b) where
  negate = fmap negate
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  fromInteger = pure . fromInteger
  abs = fmap abs
  signum = fmap signum

sortByKey' :: (Ord b) => (a -> b) -> [a] -> [a]
sortByKey' key = sortBy (compare `on` key)

groupByKey' :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupByKey' key = groupBy ((==) `on` key)

sortAndGroupByKey' :: (Ord b) => (a -> b) -> [a] -> [[a]]
sortAndGroupByKey' key = groupByKey' key . sortByKey' key

sumof :: (Num b) => (a -> b) -> ([a] -> b)
sumof f = (foldr (+) 0) . (map f)

productof :: (Num b) => (a -> b) -> ([a] -> b)
productof f = (foldr (*) 1) . (map f)

meanof :: (Real b, Fractional c) => (a -> b) -> ([a] -> c)
meanof f = \xs -> realToFrac (sumof f xs) / genericLength xs


reduce :: (NFunctor1 NList n) => NList (Succ n) a -> ([a] -> b) -> NList n b
reduce xs f = pmap1' (nmap f) xs

select :: (NFunctor0 NList n) => NList n a -> (a -> b) -> NList n b
select xs f = zmap0' (nmap f) xs

produce :: (NFunctor0 NList n) => NList n a -> (a -> [b]) -> NList (Succ n) b
produce xs f = smap0' (nmap f) xs


orderby :: (NFunctor1 NList n, Ord b) => NList (Succ n) a -> (a -> b) -> NList (Succ n) a
orderby xs key = zmap1' (nmap (sortByKey' key)) xs

groupby :: (NFunctor1 NList n, Ord b) => NList (Succ n) a -> (a -> b) -> NList (Succ (Succ n)) a
groupby xs key = smap1' (nmap (groupByKey' key)) xs


data Person = Person { name :: String, age :: Integer, gender :: String, status  :: String } deriving Show

persons = fromList' [Person {name="Alice", age=20, gender="F", status="Good"}, Person {name="Bob", age=18, gender="M", status="Good"}, Person {name="Chuck", age=16, gender="M", status="Bad"}] :: NList (Succ Zero) Person
