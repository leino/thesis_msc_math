module Transformed where

import Algebra.Structures.IntegralDomain
import Algebra.Matrix
import Invertible
import MatrixAux

data Ring r => Transformed r =
  Transformed (Invertible r) (Matrix r) [[r]]

elementary :: IntegralDomain r =>
  r -> Int -> Int -> Transformed r -> Transformed r
elementary q i j (Transformed (Invertible fs gs hs) t hss) =
  let (m,m') = (elemM n q i j, elemM n (neg q) i j) where n = length fs in
  Transformed (Invertible (fs `rmulmat` m) (m' `matmulc` gs) hs)
              (t`mulM`m) (hss `mmulmat` m)

switch :: IntegralDomain r =>
  Int -> Int -> Transformed r -> Transformed r
switch i j
  | i == j = id
  | otherwise = elementary one i j .
                elementary (neg one) j i .
                elementary one i j
