{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Data.Nested.List (module Data.Nested.List, module Data.Nested.Functor) where

import qualified Data.Function as F
import qualified Data.List as L

import Data.Nested.Naturals
import Data.Nested.Functor


type NList = MkNested []

sortByKey' :: (Ord b) => (a -> b) -> [a] -> [a]
sortByKey' key = L.sortBy (compare `F.on` key)

groupByKey' :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupByKey' key = L.groupBy ((==) `F.on` key)

sortAndGroupByKey' :: (Ord b) => (a -> b) -> [a] -> [[a]]
sortAndGroupByKey' key = groupByKey' key . sortByKey' key

select = nlift :: (a -> b) -> NList Zero a -> NList Zero b
produce = nlift :: (a -> [b]) -> NList Zero a -> NList (Succ Zero) b
reduce = nlift :: ([a] -> b) -> NList (Succ Zero) a -> NList Zero b

select' = nlift :: ([a] -> [b]) -> NList (Succ Zero) a -> NList (Succ Zero) b
produce' = nlift :: ([a] -> [[b]]) -> NList (Succ Zero) a -> NList (Succ (Succ Zero)) b
reduce' = nlift :: ([[a]] -> [b]) -> NList (Succ (Succ Zero)) a -> NList (Succ Zero) b

filterBy :: (a -> Bool) -> NList (Succ Zero) a -> NList (Succ Zero) a
filterBy f = select' (filter f)

orderBy :: (Ord b) => (a -> b) -> NList (Succ Zero) a ->  NList (Succ Zero) a
orderBy f = select' (sortByKey' f)

groupBy :: (Ord b) => (a -> b) -> NList (Succ Zero) a -> NList (Succ (Succ Zero)) a
groupBy f = produce' (sortAndGroupByKey' f)

ungroup = reduce' (foldl (++) [])
