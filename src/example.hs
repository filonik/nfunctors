{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Example where

import Prelude hiding (not,(&&),(||),(==),(/=),(<=),(>=),(<),(>))
import qualified Prelude as P

import Control.Arrow
import Control.Applicative
import Data.Function
import Data.List

import NLists.GenericPrelude
import NLists.Naturals
import NLists.NFunctors
import NLists

-- Example: SQL-like embedded DSL
-- Problem: https://byorgey.wordpress.com/2007/08/16/mapping-over-a-nested-functor/

class Selectable t m0 m1 n0 n1 | m0 n0 n1 -> m1, m0 m1 n0 -> n1 where
  fmap' :: (t m0 a -> t m1 b) -> (t n0 a) -> t n1 b
  (<$$>) :: (t n0 a) -> (t m0 a -> t m1 b) -> t n1 b
  (<$$>) = flip fmap'

instance (NFunctor NList Zero n) => Selectable NList Zero Zero n n where fmap' = zmap'
instance (NFunctor NList Zero n) => Selectable NList Zero (Succ Zero) n (Succ n) where fmap' = smap'
instance (NFunctor NList Zero n) => Selectable NList (Succ Zero) Zero (Succ n) n where fmap' = pmap'

instance (NFunctor NList (Succ Zero) n) => Selectable NList (Succ (Zero)) (Succ (Zero)) n n where fmap' = zmap'
instance (NFunctor NList (Succ Zero) n) => Selectable NList (Succ (Zero)) (Succ (Succ (Zero))) n (Succ n) where fmap' = smap'
instance (NFunctor NList (Succ Zero) n) => Selectable NList (Succ (Succ (Zero))) (Succ (Zero)) (Succ n) n where fmap' = pmap'


sortByKey' :: (Ord b) => (a -> b) -> [a] -> [a]
sortByKey' key = sortBy (compare `on` key)

groupByKey' :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupByKey' key = groupBy ((==) `on` key)

sortAndGroupByKey' :: (Ord b) => (a -> b) -> [a] -> [[a]]
sortAndGroupByKey' key = groupByKey' key . sortByKey' key

minOf :: (Ord b) => (a -> b) ->([a] -> b)
minOf f = minimum . (map f)

maxOf :: (Ord b) => (a -> b) ->([a] -> b)
maxOf f = maximum . (map f)

sumOf :: (Num b) => (a -> b) -> ([a] -> b)
sumOf f = (foldr (+) 0) . (map f)

prodOf :: (Num b) => (a -> b) -> ([a] -> b)
prodOf f = (foldr (*) 1) . (map f)

mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs) / genericLength xs

meanOf :: (Real b, Fractional c) => (a -> b) -> ([a] -> c)
meanOf f = mean . (map f)


select = nmap :: (a -> b) -> NList Zero a -> NList Zero b
produce = nmap :: (a -> [b]) -> NList Zero a -> NList (Succ Zero) b
reduce = nmap :: ([a] -> b) -> NList (Succ Zero) a -> NList Zero b

select' = nmap :: ([a] -> [b]) -> NList (Succ Zero) a -> NList (Succ Zero) b
produce' = nmap :: ([a] -> [[b]]) -> NList (Succ Zero) a -> NList (Succ (Succ Zero)) b
reduce' = nmap :: ([[a]] -> [b]) -> NList (Succ (Succ Zero)) a -> NList (Succ Zero) b

filterby :: (a -> Bool) -> NList (Succ Zero) a -> NList (Succ Zero) a
filterby f = select' (filter f)

orderby :: (Ord b) => (a -> b) -> NList (Succ Zero) a ->  NList (Succ Zero) a
orderby f = select' (sortByKey' f)

groupby :: (Ord b) => (a -> b) -> NList (Succ Zero) a -> NList (Succ (Succ Zero)) a
groupby f = produce' (groupByKey' f)



data Person = Person { name :: String, age :: Integer, gender :: String, status  :: String } deriving Show

persons = fromList' [Person {name="Alice", age=20, gender="F", status="Good"}, Person {name="Bob", age=18, gender="M", status="Good"}, Person {name="Chuck", age=16, gender="M", status="Bad"}] :: NList (Succ Zero) Person
