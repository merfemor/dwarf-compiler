module Main where

import Parser
import Text.ParserCombinators.Parsec
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        putStrLn "usage: dwsc <source_file.dws>"
    else
        let file = head args in do
            content <- readFile file
            res <- case parse programTree "" content of
                Left e  -> return $ show e
                Right s -> return $ show s
            putStrLn res