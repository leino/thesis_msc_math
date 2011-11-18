{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Example_6 where

import Algebra.UPoly
import Algebra.Z
import Algebra.Structures.Field
import Algebra.Structures.Module
import Algebra.TypeChar.Char hiding (Z)
import UPolyAux hiding (t, (*>))
import Testing

-- Example 9.2.6 in Fabianska-Quadrat


type Zu = UPoly Z U_
type Zut = UPoly Zu T_
t = toUPoly [zero, one]
u = toUPoly [toUPoly [zero,one]]

mon :: Zut
mon = t<^>2

fs :: [Zut]
fs = [(3::Z)*>u <+> one,
      t <+> t<*>u <+> u<^>2]

gs :: [Zut]
gs = [((neg 9::Z)*>t) <+> ((18::Z)*>t<^>2) <+> ((18::Z)*>t<*>u) <-> ((3::Z)*>u) <+> one,
      ((neg 54::Z)*>t) <+> ((9::Z)*>one)]

-- we of course reduce mod mon:
fs' = map (\p -> p `monMod` mon) fs
gs' = map (\p -> p `monMod` mon) gs

test_Example_6 = test mon fs' gs'
