module Syntax.Error where

import Syntax.Abstract(Type)

data CompilationError = UndefinedVariable String 
                      | UndefinedFunction String
                      | DuplicateVariableDefinition String
                      | DuplicateFunctionDefinition String
                      | TypeMismatch { expectedType :: Type, actualType :: Type } 
                      | BadArgumentNumber String deriving Show
