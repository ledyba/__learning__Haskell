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
import Data.Maybe ( Maybe(..) )

mfail :: Maybe t
mfail = Nothing

may :: String -> Maybe String
may z = do return "abc"
           def <- mfail
           return (z ++ def)

main = case (may "here") of
	Just z -> putStrLn(z)
	Nothing -> putStrLn("??")


