module Main where

import Parser
import Printer
import Translator
import ByteCodeTranslator
import Text.ParserCombinators.Parsec
import System.Environment
import Data.Binary.Put(runPut)
import System.FilePath.Posix(replaceExtension)
import qualified Data.ByteString.Lazy as BS


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
                    --putStrLn $ show res ++ "\n\n"
                    case abstractToTranslatable res of
                        Left e     -> putStrLn $ show e
                        Right trtd ->
                            let bc = toByteCode trtd
                                bc' = preTransform bc
                                bstr = putProgram bc' 
                                ofile = replaceExtension file ".dwc"
                            in do
                            --putStrLn $ show trtd ++ "\n\n" ++ show bc'
                            BS.writeFile ofile (runPut bstr)
                            
