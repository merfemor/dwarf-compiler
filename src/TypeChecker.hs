module TypeChecker(checkFunctions) where

import Syntax.Translatable
import Syntax.Abstract(Type)
import Syntax.Error
import Data.List(elemIndex)
import Data.Maybe(fromJust)

fromEitherList :: [Either a b] -> Either a [b]
fromEitherList []     = Right []
fromEitherList (e:es) = do
    b <- e
    bs <- fromEitherList es
    return (b:bs)

    
checkStatement :: Id -> Statement -> [Function] -> Either CompilationError ()
checkStatement _ _ _ = Right ()


checkFunction :: Function -> [Function] -> Either CompilationError ()
checkFunction f fs = let fid = fromJust $ elemIndex f fs in 
                         fromEitherList (map (\s -> checkStatement fid s fs) (functionBody f)) >> Right ()
        

checkFunctions :: [Function] -> Either CompilationError ()
checkFunctions fs = fromEitherList (map (\f -> checkFunction f fs) fs) >> Right ()
