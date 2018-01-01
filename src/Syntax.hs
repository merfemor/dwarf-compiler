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

builtInTypes :: Map.Map String Type
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

data Statement = VarDef Var
               | VarAssign String Expression
               | FuncDef Function
               | WhileLoop Expression [Statement]
               | IfElse Expression [Statement] [Statement]
               | Return (Maybe Expression)
               | FuncCall FunctionCall deriving Show

type ProgramTree = [Function]

type FunctionBody = [Statement]

languageDef :: LanguageDef a
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
                                      , "int"
                                      , "double"
                                      , "string"
                                      , "void"
                                      ]
            , Token.reservedOpNames = Map.keys binaryOperations ++ Map.keys unaryOperations
            }
