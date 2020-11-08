module Main where

import System.Exit (exitFailure)

main :: IO ()
main = do
    putStrLn "This test always fails!"
    exitFailure
