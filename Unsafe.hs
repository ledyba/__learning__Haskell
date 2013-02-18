{-# LANGUAGE CPP #-}
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

import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
	a <- return $ unsafePerformIO $ putStrLn "case A" -- 使ってないので評価されない
	b <- return $ unsafePerformIO $ (putStrLn "case B" >> return "b") -- 使ってるので評価される
	_ <- getLine
	putStrLn $ "result: " ++ b
	return ()
