module TypeChecker(checkFunctionsTypeErrors) where

import Syntax.Translatable
import Syntax.Abstract(Type)
import Syntax.Error

type CheckResult = Either CompilationError ()


checkMany :: (a -> [Function] -> CheckResult) -> [a] -> [Function] -> CheckResult
checkMany _ []     _   = Right ()
checkMany f (x:xs) fns = f x fns >> checkMany f xs fns


checkStatement :: Statement -> [Function] -> CheckResult
checkStatement = undefined


checkFunctionsTypeErrors :: [Function] -> CheckResult
checkFunctionsTypeErrors fs = helper fs fs where
    helper []     _   = Right ()
    helper (f:fs) afs = (checkMany checkStatement) (functionBody f) afs >> helper fs afs 
