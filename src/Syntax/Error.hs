module Syntax.Error where

import Syntax.Abstract(Type, Function, Var)

data CompilationError = UndefinedVariable String 
                      | UndefinedFunction String
                      | DuplicateVariableDefinition Var
                      | DuplicateFunctionDefinition Function
                      | TypeMismatch { expectedType :: Type, actualType :: Type } 
                      | BadArgumentNumber String deriving Show
