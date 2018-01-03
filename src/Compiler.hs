module Main where

import Parser
import Translator
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
            case parse abstractProgramTree file content of
                Left e    -> putStrLn $ show e
                Right res -> do
                    putStrLn $ show res ++ "\n\n"
                    case abstractToTranslatable res of
                        Left e     -> putStrLn $ show e
                        Right trtd -> putStrLn $ show trtd
