{-# LANGUAGE CPP, TemplateHaskell, Arrows #-}
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

import Control.Monad (unless)
import Control.Monad.Cont
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import Control.Arrow
       (ArrowZero(..), ArrowPlus(..), (>>^), (^>>), ArrowChoice(..),
        (>>>), Arrow(..), Kleisli(..), zeroArrow, returnA, Arrow)
import qualified Control.Arrow as Control (Arrow)
type ArrowIO = Kleisli IO

newtype State s a = State (s,a)
return a = \s -> State (s,a)

expr' :: Kleisli [] Integer Integer
expr' = proc x -> do
        returnA -< (x)
        <+> do
        returnA -< (1)

exeMain :: IO ()
exeMain = do
    putStrLn()

callProc pr msg = callCC $ \ k -> do
   return (pr k msg)

rz pr = do
    a <- return "String"
    callProc pr a

rc :: Cont r String
rc = do
    a <- return "String"
    z <- callCC $ \ k-> do
        if (length a) >= 100 then (return 1) else (return 2)
    return $  a++"a"++show(z)

ra :: (->) Int Int
ra = proc x -> do
    returnA -< (x+1 :: Int)

putStrLnA :: ArrowIO String ()
putStrLnA = Kleisli putStrLn

putStrLnA_AddX :: ArrowIO String ()
putStrLnA_AddX = (arr (++"X")) >>> putStrLnA

putStrLnA_AddY :: ArrowIO String ()
putStrLnA_AddY = (arr (++"Y") )>>> putStrLnA

putStrLnA_AddZ :: ArrowIO String ()
putStrLnA_AddZ = (arr (++"Z") )>>> putStrLnA

runA :: ArrowIO () ()
runA =
    (const "String") ^>>
    putStrLnA_AddX &&& putStrLnA_AddY &&& putStrLnA_AddZ >>^
    (const ())

--exeMain :: IO ()
--exeMain = runKleisli runA ()
{-
exeMain = return (runCont rc id) >>= \ res ->
    putStrLn "YEAH" >>
    putStrLn res >>
    return ()
-}
testMain = do
    putStrLn "Hello"

#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

