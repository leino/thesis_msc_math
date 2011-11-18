{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Example_2 where

import Algebra.UPoly
import Algebra.Q
import Algebra.Structures.Field
import Algebra.TypeChar.Char hiding (Q)
import UPolyAux hiding (t)
import Testing

-- Example 9.2.2 in Fabianska-Quadrat


type Qu = UPoly Q U_
type Qut = UPoly Qu T_
t = toUPoly [zero, one]
u = toUPoly [toUPoly [zero,one]]

mon :: Qut
mon = t<+>u<-> ((2::Q)*>one*>one)

fs :: [Qut]
fs = [((2::Q)*>one)*>t<*>u, t<^>2 <*> u <+> one]

-- what is the inverse?
gs :: [Qut]
gs = [neg $ ((1/2)::Q)*>one*>t, one]

-- we of course reduce mod mon:
fs' = map (\p -> p `monMod` mon) fs
gs' = map (\p -> p `monMod` mon) gs

test_Example_2 = test mon fs' gs'
