module Syntax.Translatable where

import Syntax.Abstract (UnaryOperation, BinaryOperation, Type, Var)

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
               | StFCall Id [Expression] deriving Show

-- string pool and function list
type TranslatableProgramTree = ([String], [Function])
