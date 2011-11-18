module Suslin where

import Control.Monad.State
import Control.Monad.Writer
import Algebra.Structures.CommutativeRing
import Algebra.UPoly
import Algebra.Matrix
import Algebra.TypeChar.Char
import DynamicIdeal
import Invertible
import Transformed
import UPolyAux
import MatrixAux

shed :: (CommutativeRing r, Eq r) =>
  [r] -> [UPoly r T_] -> Transformed (UPoly r T_) ->
  DynamicIdealT r (Writer [Matrix (UPoly r T_)])
                  (Transformed (UPoly r T_))
shed ws dfs (Transformed (Invertible fs gs hs) t hss) = do
  as <- fmap (map liftCoeff) get
  let dhss = (map liftCoeff ws) `ctensorr` dfs
  return $ Transformed (Invertible (fs`rsub`(as`rmulm`dhss))
                       gs (hs`ucsub`(dhss`mmulc`gs))) t (hss`umadd`dhss)

cancel :: (CommutativeRing r, Eq r) =>
  Int -> r -> Transformed (UPoly r T_) -> Transformed (UPoly r T_)
cancel i b row@(Transformed (Invertible fs _ _) _ _) =
  foldr1 (.) [elementary (neg (b<*>(lC f))*>(t<^>((deg f)-(deg$fs!!i))))
                         i j
             |(f,j) <- zip fs [0 ..], f/=zero, j/=i] row

leaf :: (CommutativeRing r, Eq r, Show r) =>
  UPoly r T_ -> (Transformed (UPoly r T_)) ->
  StateT [r] (Writer [Matrix (UPoly r T_)]) (Invertible r)
leaf mon r@(Transformed (Invertible fs gs hs) gamma hss) = do
  as <- get
  tell [gamma]
  let (y,x,ws) = (norm mon (head fs), norm mon (head gs),
                  normWitnesses mon one as hs)
      ws' = normWitnesses mon (head fs) as (hss `mmulc` (basis (length fs) 0))
  return$Invertible [y<+>(as`apply`ws')] [x] (ws`cadd`(x`scalec`ws'))

suslin :: (CommutativeRing r, Eq r) =>
  Transformed (UPoly r T_) ->
  DynamicIdealT r (Writer [Matrix (UPoly r T_)])
                  (Transformed (UPoly r T_))
suslin row@(Transformed (Invertible fs _ _) _ _) = do
  let (d,i) = minimum [(deg f,i) | (f,i) <- zip fs [0 ..], f /= zero]
      n = length fs
  if (length$filter (/= zero) fs) == 1
    then return$switch i 0 row
    else do
      ad <- decide$lC$fs!!i
      case ad of
        Left ws -> shed ws ((t<^>d)`scaler`(basis n i)) row >>= suslin
        Right (Invertible [b] _ ws) ->
          (return$cancel i b row) >>=
            shed ws [if j/=i then neg$lT f else zero
                    |(f,j)<-zip fs [0 ..]] >>= suslin

runSuslin :: (CommutativeRing r, Eq r, Show r) =>
  UPoly r T_ -> [UPoly r T_] -> [UPoly r T_] ->
  (Invertible r, [Matrix (UPoly r T_)])
runSuslin mon fs gs =
  let n = length fs in
  runWriter $ runDynamicIdealT (suslin (Transformed (Invertible fs gs [])
                                 (identity n) [replicate n zero]))
                               (leaf mon)
