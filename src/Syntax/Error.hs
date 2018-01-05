module Syntax.Error where

import Syntax.Abstract(Type, Function, Var)

data TypeMismatch = TypeMismatch { expectedType :: Maybe Type, actualType :: Maybe Type } deriving Show

data CompilationError = UndefinedVariable String 
                      | UndefinedFunction String
                      | DuplicateVariableDefinition Var
                      | DuplicateFunctionDefinition Function
                      | ReturnTypeMismatch String TypeMismatch
                      | VarAssignTypeMismatch String TypeMismatch
                      | BadArgumentNumber String deriving Show
