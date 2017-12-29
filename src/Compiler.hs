module Main where

import Parser
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        putStrLn "usage: dwsc <source_file.dws>"
    else
        let file = head args in do
            content <- readFile file
            putStrLn content
