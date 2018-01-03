module Syntax.Error where

import Syntax.Abstract(Type, Function)

data CompilationError = UndefinedVariable String 
                      | UndefinedFunction String
                      | DuplicateVariableDefinition String
                      | DuplicateFunctionDefinition Function
                      | TypeMismatch { expectedType :: Type, actualType :: Type } 
                      | BadArgumentNumber String deriving Show
