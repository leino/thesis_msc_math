{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module UPolyAux where

import Data.List hiding (transpose)
import Algebra.Structures.CommutativeRing
import Algebra.UPoly
import Algebra.Matrix

import MatrixAux

(*>) :: (CommutativeRing r, Eq r) => r -> UPoly r x -> UPoly r x
(*>) a f = (liftCoeff a) <*> f

lC :: (CommutativeRing r, Eq r) => (UPoly r x) -> r
lC f = lt f
lT :: (CommutativeRing r, Eq r) => (UPoly r x) -> (UPoly r x)
lT f = (lC f) *> (t<^>(deg f))

t :: (CommutativeRing r, Eq r) => UPoly r x
t = toUPoly [zero, one]

isMonic :: (CommutativeRing r, Eq r) => UPoly r x -> Bool
isMonic f = lt f == one

-- norm of g wrt f
norm :: (CommutativeRing r, Eq r) => (UPoly r x) -> (UPoly r x) -> r
norm f g
  | isMonic f == False = error "norm: f must be monic"
  | otherwise = 
      det m
      where
      d = deg f
      m = transpose $ matrix $ [pad . coeffs $ (g<*>t<^>k) `monMod` f
                               |k <- [0 .. d-1]]
      pad cs = cs ++ genericReplicate (d - genericLength cs) zero
      coeffs (UP cs) = cs

-- returns xs such that N(g + hs.as) = N(g) + as.xs
normWitnesses :: (CommutativeRing r, Eq r) => UPoly r x -> UPoly r x -> [r] -> [UPoly r x] -> [r]
normWitnesses mon g [] _ = []
normWitnesses mon g [a] [h] = 
  [(shift . norm mon' $ g' <+> s*>h') `evaluate` a]
  where
  s = toUPoly [zero, one] -- indeterminate in place of a
  (h', g', mon') = (mapPoly liftCoeff h, mapPoly liftCoeff g, mapPoly liftCoeff mon) -- lift to R[s][t]
  shift (UP cs) = toUPoly $ drop 1 cs -- get coefficient of s

normWitnesses mon g as hs =
  (normWitnesses mon g asL hsL)
  ++
  (normWitnesses mon (g<+>(sumRing [a*>h|(a,h)<-zip asL hsL])) asR hsR)
  where
  ((asL, asR),(hsL, hsR)) = (splitHalf as, splitHalf hs)
  splitHalf xs = 
    let n = length xs `div` 2 in
    (take n xs, drop n xs)

monMod g f -- g modulo f, where f is monic
  | isMonic f == False = error "f must be monic"
  | deg f <= deg g = (g <-> ((lC g) *> f <*> t<^>((deg g) - (deg f))) ) `monMod` f
  | otherwise = g

evaluate :: (CommutativeRing r, Eq r) => UPoly r x -> r -> r
evaluate f@(UP cs) a
  | f == zero = zero
  | otherwise = foldr1 (<+>) (zipWith (<*>) cs [a<^>i | i <- [0 ..]])

liftCoeff :: (CommutativeRing r, Eq r) => r -> UPoly r x
liftCoeff c = toUPoly [c]

dropCoeff :: CommutativeRing r => UPoly r x -> r
dropCoeff (UP []) = zero
dropCoeff (UP [c]) = c

mapPoly f (UP cs) = toUPoly $ map f cs
