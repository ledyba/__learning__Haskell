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

data Event = ForceRedraw
data Response = DrawText String
newtype Kanade world =　Kanade ((Event, world) -> (Response, world))

data Logic a b c = Logic { runStep :: (a -> (LogicSignal b, Logic b)) } | EPS
data LogicSignal b = Yield b (Logic a b c) | Exit a | Value a (Logic a b c) deriving (Show, Eq)

(>>=) :: Logic a b c -> (b -> Logic a b c) -> Logic a b c
g >>= f = runS

mainLogic =
    (Logic $ \x -> (Yield (x+1), EPS)) `merge`
    (Logic $ \x -> (Yield (x+2), EPS))

-- Hello World
exeMain = do
    (a, x) <- return (runStep mainLogic 3)
    putStrLn ("hello"++"World"++(show a))
    (b, _) <- return (runStep x 3)
    putStrLn ("hello"++"World"++(show b))

-- Entry point for unit tests.
testMain = do
    putStrLn("Yeah")

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

