module Syntax where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Var = IVar Int | DVar Double | SVar String deriving Show
data NamedVar = NamedVar { name :: String, var :: Var } deriving Show

languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "if"
                                      , "else"
                                      , "while"
                                      , "return"
                                      , "or"
                                      , "int"
                                      , "double"
                                      , "string"
                                      , "void"
                                      , "print"
                                      {-
                                        TODO: check if:
                                         1. type names int, double, string, void here?
                                         2. print here?
                                      -}
                                      ]
            , Token.reservedOpNames = ["+", "-", "*", "/", "=",
                                       "<", ">", ">=", "<=", "not", "is",
                                       "and", "or", "not"
                                      ]
            }