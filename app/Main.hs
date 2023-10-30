module Main (main) where

import Prelude hiding ((+))
import Roman ((+), romanNumeral)
import Data.Function ((&))
import System.Environment (getArgs)

main :: IO ()
main = do
    arguments <- getArgs
    map romanNumeral arguments
        & foldl (+) []
        & concatMap show
        & putStrLn
