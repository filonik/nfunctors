{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Data.Generic.Functor where

infixl 4 <<$>> -- Same as <$> in Functor
infixl 4 <<$$>> -- Same as <$> in Functor
--infixl 4 <<*>> -- Same as <*> in Applicative
--infixl 4 <<**>> -- Same as <*> in Applicative

class GenericFunctor e f g h | e f g -> h, e f h -> g where
  nmap :: (e a -> f b) -> g a -> h b
  (<<$>>) :: (e a -> f b) -> g a -> h b
  (<<$$>>) :: g a -> (e a -> f b) -> h b
  (<<$>>) = nmap
  (<<$$>>) = flip (<<$>>)

