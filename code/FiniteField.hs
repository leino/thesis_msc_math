{-# LANGUAGE TypeSynonymInstances #-}

module FiniteField where

import Test.QuickCheck
import Math.Algebra.Field.Base
import Math.Common.IntegerAsType
import Algebra.Structures.CommutativeRing

instance (IntegerAsType n) => Ring (Fp n) where
  (<+>) a b = a + b
  (<*>) a b = a * b
  neg a = -a 
  one = 1
  zero = 0

instance (IntegerAsType n) => CommutativeRing (Fp n)


instance Arbitrary F2 where
  arbitrary = elements f2

instance Arbitrary F3 where
  arbitrary = elements f3

instance Arbitrary F5 where
  arbitrary = elements f5

instance Arbitrary F7 where
  arbitrary = elements f7

instance Arbitrary F11 where
  arbitrary = elements f11
