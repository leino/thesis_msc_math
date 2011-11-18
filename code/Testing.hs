module Testing where

import Algebra.UPoly
import Algebra.Structures.CommutativeRing
import Algebra.Z
import Algebra.TypeChar.Char hiding (Z)
import MatrixAux
import Invertible
import Suslin
import Control.Monad
import UPolyAux
import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random
import Math.Algebra.Field.Base
import FiniteField

-- parameters. safe to modify
n = 3
d = 3
type BaseRing = F11


-- automated testing -----------------

-- check that suslin works
prop_suslin :: SuslinInput BaseRing -> Bool
prop_suslin (SuslinInput mon fs gs)
  | (not $ validInput mon fs gs) = error "prop_suslin: invalid input"
  | otherwise = let (Invertible ys xs _, gammas) = runSuslin mon fs gs
                in validOutput mon fs ys xs gammas



-- manual testing with IO -----------------

test mon fs gs = do
  unless (validInput mon fs gs) $ error "input not valid"
  putStrLn $ "monic: " ++ show mon
  putStrLn $ "fs: " ++ show fs
  putStrLn $ "gs: " ++ show gs
  putStrLn $ "running suslin..."
  let (Invertible ys xs _, gammas) = runSuslin mon fs gs
  putStrLn $ "checking output..."
  unless (validOutput mon fs ys xs gammas) $ error "output not valid"
  putStrLn $ "output ok!"
  putStrLn $ "gammas: "
  putStrLn $ show gammas
  putStrLn $ "ys: "
  putStrLn $ show ys

-- general testing facilities -----------------

validInput mon fs gs =
  and [(fs `apply` gs) `monMod` mon == one,
       isMonic mon]

validOutput mon fs ys xs gammas =
  let ys' = [norm mon (head $ fs `rmulmat` gamma) | gamma <- gammas]
  in and [ys == ys', ys `apply` xs == one]

-- generators -----------------

-- now we define the input generator
data (CommutativeRing r, Eq r, Arbitrary r) =>
  SuslinInput r = SuslinInput (UPoly r T_) [UPoly r T_] [UPoly r T_]
  deriving Show

-- modify here to get different n and d
instance (CommutativeRing r, Eq r, Arbitrary r) => Arbitrary (SuslinInput r) where
  arbitrary = suslinInputGen n d

-- generate a testcase for suslin
suslinInputGen n d = do
  mon <- monGen d
  fs <- rowGen (n-1) (d-1)
  gs <- rowGen (n-1) (d-1)
  let r = (fs `apply` gs) `monMod` mon -- remainder
  return $ SuslinInput mon (fs ++ [neg r <+> one]) (gs ++ [one]) -- compensate for remainder

-- generate a monic polynomial of degree n
monGen :: (CommutativeRing r, Eq r, Arbitrary r) => Int -> Gen (UPoly r x)
monGen d = 
  fmap (\cs -> toUPoly (cs ++ [one])) (vectorOf d arbitrary)
-- generate a polynomial of degree at most d
polyGen :: (CommutativeRing r, Eq r, Arbitrary r) => Int -> Gen (UPoly r x)
polyGen d = do
 fmap toUPoly (vectorOf (d+1) arbitrary)
-- generate an n element row of polynomials of degrees at most d
rowGen :: (CommutativeRing r, Eq r, Arbitrary r) => Int -> Int -> Gen [UPoly r x]
rowGen n d =
  vectorOf n (polyGen d)
