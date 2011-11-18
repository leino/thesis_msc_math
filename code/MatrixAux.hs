module MatrixAux where

import Algebra.Matrix
import Algebra.Structures.Ring
import Algebra.Structures.IntegralDomain

unRowM = unVec . matrixToVector
unColM = unRowM . transpose

apply :: (Ring r, Eq r) => [r] -> [r] -> r
apply xs ys
  | length xs /= length ys = error "apply: bad dim"
  | xs == [] = zero
  | otherwise = let [x] = unRowM $ (matrix [xs]) `mulM` (transpose . matrix $ [ys]) in x

rmulm :: Ring r => [r] -> [[r]] -> [r]
rmulm xs xss = if length xs /= length xss then error ("rmulm: bad dim" ++ (show (length xs)) ++ (show (length xss)) ++ (show (length (head xss)))) else
  unRowM $ (matrix [xs]) `mulM` (matrix xss)

matmulc :: Ring r => Matrix r -> [r] -> [r]
matmulc m xs = if length xs /= (snd . dimension $ m) then error "matmulc: bad dim" else
  unColM $ m `mulM` (transpose . matrix $ [xs])

mmulmat :: Ring r => [[r]] -> Matrix r -> [[r]]
mmulmat xss m = if (length$head xss) /= (fst . dimension $ m) then error "mmulmat: dim error" else
  unMVec $ (matrix xss) `mulM` m

mmulc :: Ring r => [[r]] -> [r] -> [r]
mmulc xss xs = if length xs /= (length . head $ xss) then error "mmulc: bad dim" else
  unColM $ (matrix xss) `mulM` (transpose . matrix $ [xs])

madd :: Ring r => [[r]] -> [[r]] -> [[r]]
madd xss yss = if (length$head xss) /= (length$head yss) || length xss /= length yss then error "madd: dim error" else
  [[x <+> y | (x,y) <- zip xs ys] | (xs,ys) <- zip xss yss]

rsub :: Ring r => [r] -> [r] -> [r]
rsub xs ys = if length xs /= length ys then error "rsub: bad dim" else
  unRowM $ (matrix [xs]) `addM` (matrix [map neg ys])

csub :: Ring r => [r] -> [r] -> [r]
csub xs ys = if length xs /= length ys then error "csub: bad dim" else
  unRowM $ (matrix [xs]) `addM` (matrix [map neg ys])

radd :: Ring r => [r] -> [r] -> [r]
radd xs ys = if length xs /= length ys then error "radd: bad dim" else
  unRowM $ (matrix [xs]) `addM` (matrix [ys])

cadd :: Ring r => [r] -> [r] -> [r]
cadd xs ys = if length xs /= length ys then error "cadd: bad dim" else
  unRowM $ (matrix [xs]) `addM` (matrix [ys])

-- warning: this cannot check for dimensions
ctensorr xs ys = [[x<*>y|y<-ys]|x<-xs]

-- unsafe rsub: if the vectors are different lengths, we simply pad
-- the shorter one with zeroes.
-- the restriction is that the shorter one needs to be on the left
ursub :: Ring r => [r] -> [r] -> [r]
ursub xs ys
  | length xs > length ys = error "ursub: lower dimensional vector needs to be on the left"
  | otherwise = zipWith (<->) (xs ++ repeat zero) ys

ucsub :: Ring r => [r] -> [r] -> [r]
ucsub xs ys
  | length xs > length ys = error "ucsub: lower dimensional vector needs to be on the left"
  | otherwise = zipWith (<->) (xs ++ repeat zero) ys

-- will padd the number of rows with zero rows
umadd :: Ring r => [[r]] -> [[r]] -> [[r]]
umadd xss yss =
  if length xss > length yss
    then error "umadd: lower dimensional matrix needs to be on left"
    else if length (head xss) /= length (head yss)
           then error "umadd: matrices must have the same number of collumns"
           else let zs = replicate (length $ head xss) zero
                    zss = replicate ((length yss) - (length xss)) zs --padding
                    xss' = xss ++ zss in --padded xss
                    xss' `madd` yss

rmulmat :: Ring r => [r] -> Matrix r -> [r]
rmulmat xs m = if length xs /= (fst . dimension $ m) then error "rmulmat: bad dim" else
  unRowM $ (matrix [xs]) `mulM` m

scaler :: Ring r => r -> [r] -> [r]
scaler c xs = [c <*> x | x <- xs]

scalec :: Ring r => r -> [r] -> [r]
scalec c xs = [c <*> x | x <- xs]

basis n i = (replicate i zero) ++ one:(replicate (n-(i+1)) zero)

elemM :: IntegralDomain r => Int -> r -> Int -> Int -> Matrix r
elemM n q i j = addRow (identity n) (Vec $ q `scaler` (basis n j)) i


det :: Ring r => Matrix r -> r
det (M [Vec [a]]) = a
det mat
  | m /= n = error "det: non-square matrix"
  | otherwise = sumRing [sgn <*> mat!!!(0,j) <*> (det $ minor mat 0 j)
                        |(j,sgn) <- zip [0 .. n-1] (cycle [one,neg one])]
  where
  (n,m) = dimension mat

minor :: Ring r => Matrix r -> Int -> Int -> Matrix r
minor m i j =
  let rs = unMVec m in
  matrix $ map (skratch j) (skratch i rs)
  where
  skratch i rs = (take i rs) ++ (drop (i+1) rs)
