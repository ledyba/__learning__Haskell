{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Data.Maybe
import Control.Monad (MonadPlus(..), unless)
import Data.Monoid (Monoid)

newtype State s a = State {runState :: s -> (a, s)}
instance Monad (State s) where
    return a = State $ \s -> (a,s)
    (State g) >>= f = State $ \s ->
        let (a2,s2) = (g s) in (f a2) `runState` s2
add :: Int -> State Int Int
add n = State $ \ s -> (s, s+n)
dec n = State $ \ s -> (s, s-n)
modify n = State $ \ s -> (s, n)

newtype Cont r a = Cont {runCont :: (a->r) -> r}
instance Monad (Cont c) where
    return a = Cont $ \k -> k a
    -- (>>=) :: Cont r a -> (a-> Cont r b) -> Cont r b
    (Cont g) >>= f = Cont $ \ b -> g $ \ a -> (f a) `runCont` b

data Task a r = Task {runTask :: a -> (r, (Task a r))} | EPS | Yield
next r = Task $ \ _ -> (r, EPS)
yield r = Task $ \ _ -> (r, Yield)
(->>) :: Task a r -> (r -> Task a r) -> Task a r
(Task g) ->> f = Task $ \ a -> case (g a) of
    (r, EPS) -> (f r) `runTask` a
    (r, Yield) -> (r, (f r))

manip :: Task a Int
manip =
    next 2 ->> \ a ->
    yield (-1) ->> \ z ->
    next (z+2)

-- Hello World
exeMain = do
    (r, f) <- return (manip `runTask` 0)
    putStrLn ("fst: "++(show r))
    (n, _) <- return (f `runTask` 0)
    putStrLn ("nxt: "++(show n))

-- Entry point for unit tests.
testMain = do
    putStrLn ("World")

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

