module Syntax where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Data.Map as Map
import qualified Text.ParserCombinators.Parsec.Token as Token


data UnaryOperation = Not | Neg deriving (Show, Eq)

unaryOperations :: Map.Map String UnaryOperation
unaryOperations = Map.fromList [("not", Not), ("-", Neg)]

data BinaryOperation = And | Or | Eq | G | L | GE | LE | NotE | Sum | Sub | Mul | Div deriving (Show, Eq)

binaryOperations :: Map.Map String BinaryOperation
binaryOperations = Map.fromList [("and", And), ("or", Or),
                                 ("==", Eq), (">", G), ("<", L), (">=", GE), ("<=", LE), ("!=", NotE),
                                 ("+", Sum), ("-", Sub), ("*", Mul), ("/", Div)]

data Type = Int | Double | String deriving Show

builtInTypes = Map.fromList [("double", Double), ("int", Int), ("string", String)]

data Function = Function { returnType :: Maybe Type
                         , funcName :: String
                         , arguments :: [(Type, String)]
                         , functionBody :: FunctionBody
                         } deriving Show

data FunctionCall = FunctionCall String [Expression] deriving Show

data Expression = SVar String
                | NumVar Double
                | UnaryExpression UnaryOperation Expression
                | BinaryExpression BinaryOperation Expression Expression
                | FCall FunctionCall
                | VCall String
                deriving Show


data Var = Var { varType :: Type
                    , varName :: String
                    , initialValue :: Expression
                    } deriving Show

data Statement = VarDef Var deriving Show

type ProgramTree = [Function]

type FunctionBody = [Statement]

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