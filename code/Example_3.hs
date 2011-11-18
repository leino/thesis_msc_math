{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Example_3 where

import Algebra.UPoly
import Algebra.Z
import Algebra.Structures.Ring
import Algebra.Structures.Module
import Algebra.TypeChar.Char hiding (Z)
import UPolyAux hiding (t, (*>))
import Testing

-- Example 9.2.3 in Fabianska-Quadrat


type Zu = UPoly Z U_
type Zut = UPoly Zu T_
t = toUPoly [zero, one] :: Zut
u = toUPoly [toUPoly [zero,one]] :: Zut

mon :: Zut
mon = t <-> (4::Z)*>u <+> (2::Z)*>one

fs :: [Zut]
fs = [t<*>u<+>t, t<+>(4::Z)*>u<^>2 <-> (2::Z)*>u<+>one]

gs :: [Zut]
gs = [neg one, one]

-- we of course reduce mod mon:
fs' = map (\p -> p `monMod` mon) fs
gs' = map (\p -> p `monMod` mon) gs

test_Example_3 = test mon fs' gs'
