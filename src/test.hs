{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Test where

import Prelude hiding (not,(&&),(||),(==),(/=),(<=),(>=),(<),(>))
import qualified Prelude as P

import Data.Generic.Prelude
import Data.Generic.Functor

import Data.Nested.Naturals
import Data.Nested.List


-- Test Data

xs = toNested [1,2,3,4] :: NList N1 Integer
ys = toNested [[1,2],[3,4]] :: NList N2 Integer
zs = toNested [[[1],[2]],[[3],[4]]] :: NList N3 Integer

data Person = Person { name :: String, age :: Integer, gender :: String, status  :: String } deriving Show

persons = toNested [Person {name="Alice", age=20, gender="F", status="Good"}, Person {name="Bob", age=18, gender="M", status="Good"}, Person {name="Chuck", age=16, gender="M", status="Bad"}] :: NList N1 Person
