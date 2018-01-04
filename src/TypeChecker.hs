module TypeChecker(checkFunctionsTypeErrors) where

import Syntax.Translatable
import Syntax.Abstract(Type)
import Syntax.Error

type CheckResult = Either CompilationError ()


checkFunction :: Function -> [Function] -> CheckResult
checkFunction fs f = undefined


checkFunctionsTypeErrors :: [Function] -> CheckResult
checkFunctionsTypeErrors fs = helper fs fs where
    helper []     _   = Right ()
    helper (f:fs) afs = checkFunction f afs >> helper fs afs
