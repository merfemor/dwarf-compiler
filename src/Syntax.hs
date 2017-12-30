module Syntax where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Data.Map as Map
import qualified Text.ParserCombinators.Parsec.Token as Token


data UnaryOperation = Not | Neg deriving Show

unaryOperations :: Map.Map String UnaryOperation
unaryOperations = Map.fromList [("not", Not), ("-", Neg)]

data BinaryOperation = And | Or | Eq | G | L | GE | LE | NotE | Sum | Sub | Mul | Div deriving Show

binaryOperations :: Map.Map String BinaryOperation
binaryOperations = Map.fromList [("and", And), ("or", Or),
                                 ("==", Eq), (">", G), ("<", L), (">=", GE), ("<=", LE), ("!=", NotE),
                                 ("+", Sum), ("-", Sub), ("*", Mul), ("/", Div)]

data Type = Int | Double | String deriving Show

data Statement = Statement deriving Show

data Function = Function { funcName :: String
                         , arguments :: [(String, Type)]
                         , returnType :: Maybe Type
                         , statementList :: [Statement]
                         } deriving Show

data FunctionCall = FunctionCall { function :: Function, callArguments :: [Expression]} deriving Show

data NumExpression = NumVar Double
                     | UnaryExpression UnaryOperation NumExpression
                     | BinaryExpression NumExpression BinaryOperation NumExpression
                     | FCall FunctionCall
                     | VCall String
                     deriving Show

data Expression = SVal String
                  | NumExpression deriving Show


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
            , Token.reservedOpNames = Map.keys binaryOperations ++ Map.keys unaryOperations
            }