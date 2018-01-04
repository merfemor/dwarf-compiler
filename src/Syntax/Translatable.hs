module Syntax.Translatable where

import Syntax.Abstract ( UnaryOperation
                       , BinaryOperation
                       , Type
                       , Type(Int)
                       , Type(Double)
                       , Type(String)
                       , Var
                       , Var(Var)
                       )
import Data.List(findIndex)

type Id = Int

data VariableId = VariableId { funcId :: Id
                             , varId :: Id
                             , isArgument :: Bool
                             } deriving Show

data Function = Function { returnType :: Maybe Type
                         , funcName :: String
                         , localVars :: [Var]
                         , arguments :: [Var]
                         , outerFunctionId :: Maybe Id
                         , functionBody :: [Statement]
                         } deriving Show
                         
data Expression = SLit Id
                | ILit Int
                | DLit Double
                | UnaryExpression UnaryOperation Expression
                | BinaryExpression BinaryOperation Expression Expression
                | FCall Id [Expression]
                | VCall VariableId
                deriving Show
                
data Statement = VarAssign VariableId  Expression
               | WhileLoop Expression [Statement]
               | IfElse Expression [Statement] [Statement]
               | Return (Maybe Expression)
               | FuncCall Id [Expression] deriving Show

-- string pool and function list
type TranslatableProgramTree = ([String], [Function])


standartFunctions :: [Function]
standartFunctions = [ Function Nothing       "print"    [] [Var String ""] Nothing []
                    , Function Nothing       "printn"   [] [Var Double ""] Nothing []
                    , Function (Just Int)    "dtoi"     [] [Var Double ""] Nothing []
                    ]
                    
isStandartFunction :: String -> Bool
isStandartFunction n = Nothing /= findIndex (\f -> funcName f == n) standartFunctions
