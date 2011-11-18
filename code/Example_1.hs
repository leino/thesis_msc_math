{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Example_1 where

import Algebra.UPoly
import Algebra.Q
import Algebra.Structures.Field
import Algebra.TypeChar.Char hiding (Q)
import UPolyAux hiding (t)
import Testing

-- Example 9.2.1 in Fabianska-Quadrat
-- note that the fs and mon are divided by two
-- and the gs are multiplied by two
--
-- this is simply to make sure that mon is monic.
-- one can compensate for this after the computation has been
-- carried out to get an answer for Z[x]

type Qt = UPoly Q T_
t = toUPoly [zero, one]

mon :: Qt
mon = t <-> (5/2::Q)*>one

fs :: [Qt]
fs = [(13/2::Q)*>one,
      ((1/2::Q)*>(t<^>2)) <-> ((1/2::Q)*>one)]

gs :: [Qt]
gs = [(55*2::Q)*>one <-> ((36*2::Q)*> t) <+> ((6*2::Q)*>(t<^>2)),
      neg $ (6*2::Q)*>one]

-- we of course reduce mod mon:
fs' = map (\p -> p `monMod` mon) fs
gs' = map (\p -> p `monMod` mon) gs

test_Example_1 = test mon fs' gs'
