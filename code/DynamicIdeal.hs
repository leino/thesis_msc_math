{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DynamicIdeal where

import Control.Monad.Cont
import Control.Monad.State
import Algebra.Structures.Ring
import Invertible
import MatrixAux

runDynamicIdealT (DynamicIdealT prog) invert = do
  evalStateT (runContT prog invert) []

newtype DynamicIdealT r m a =
  DynamicIdealT (ContT (Invertible r) (StateT [r] m) a)
  deriving (Functor, Monad, MonadState [r])

decide :: (Monad m, Ring r) =>
  r -> DynamicIdealT r m (Either [r] (Invertible r))
decide a = 
  DynamicIdealT $ ContT $ \k -> do
    as <- get
    let n = length as
    put $ as ++ [a]
    (Invertible xs ys zs'z) <- k $ Left $ basis (length$a:as) (length as)
    let (zs,z) = (init zs'z, last zs'z)    
    -- xs.ys = 1 + (as:a).(zs:z)
    -- a(-z) = 1 + as.zs - xs.ys
    -- a(-z) = 1 + (as++xs, zs ++ -ys)
    put $ as ++ xs
    (Invertible us vs wsts) <- k $ Right $ Invertible [neg z] [a]
                                             (zs ++ map neg ys)
    let (ws,ts) = splitAt n wsts
    -- us.vs = 1 + (as ++ xs).(ws ++ ts)
    -- us.vs = 1 + as.ws + xs.ts
    -- us.vs - xs.ts = 1 + as.ws
    -- (us ++ xs).(vs ++ -ts) = 1 + as.ws
    -- (xs ++ us).(-ts ++ vs) = 1 + as.ws
    put $ as
    return $ Invertible (xs ++ us) ((map neg ts) ++ vs) ws
