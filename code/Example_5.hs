{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Example_5 where

import Algebra.UPoly hiding (x)
import Algebra.Z
import Algebra.Structures.CommutativeRing
import Algebra.Structures.Module
import Algebra.TypeChar.Char hiding (Z)
import UPolyAux hiding (t, (*>))
import Testing

-- Example 9.2.5 in Fabianska-Quadrat,
-- due to A. van den Essen

-- We need 4 variables.
-- t is the "top variable"
-- i.e. the polynomials are viewed as polynomials
-- in t over Z[x,y,z].

type Zxyz = UPoly (UPoly (UPoly Z X_) Y_) Z_ -- the base ring
type Zxyzt = UPoly Zxyz T_ -- the polynomial ring over the base ring

t = toUPoly [zero,one] :: Zxyzt
z = toUPoly [toUPoly [zero,one]] :: Zxyzt
y = toUPoly [toUPoly [toUPoly [zero,one]]] :: Zxyzt
x = toUPoly [toUPoly [toUPoly [toUPoly [zero,one]]]] :: Zxyzt

mon :: Zxyzt
mon = (2::Z)*>t<*>x<*>y <+> t<^>2

fs :: [Zxyzt]
fs = [t<*>x<^>2,
      (2::Z)*>t<*>x<*>z
      <+>
      t<*>y<^>2
      <+>
      one]

gs :: [Zxyzt]
gs = [(-16::Z)*>y<^>4<*>x<^>2<*>z<^>2
      <->
      (16::Z)*>y<^>6<*>x<*>z
      <->
      (8::Z)*>y<*>x<*>z<^>2
      <->
      (4::Z)*>y<^>8
      <->
      (4::Z)*>y<^>3<*>z
      ,
      (2::Z)*>t<^>2<*>y<^>2<*>x<*>z
      <->
      (2::Z)*>t<*>x<*>z
      <+>
      t<^>2<*>y<^>4
      <->
      t<*>y<^>2
      <+>
      one]

-- we of course reduce mod mon:
fs' = map (\p -> p `monMod` mon) fs
gs' = map (\p -> p `monMod` mon) gs

-- this input surprisingly takes much longer
fs'' = reverse fs'
gs'' = reverse gs'

test_Example_5 = test mon fs' gs'
