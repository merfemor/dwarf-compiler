module TypeProcessor(translateFunctions) where

import Syntax.Translatable
import Syntax.Abstract(Type)
import Syntax.Error
import Data.List(elemIndex)
import Data.Maybe(fromJust)

type TypeTranslator a = a -> [Function] -> Either CompilationError a

fromEitherList :: [Either a b] -> Either a [b]
fromEitherList []     = Right []
fromEitherList (e:es) = do
    b <- e
    bs <- fromEitherList es
    return (b:bs)


translateFunction :: Id -> TypeTranslator Function
translateFunction = undefined


translateFunctions :: [Function] -> Either CompilationError [Function]
translateFunctions fs = fromEitherList $
    map (\f -> translateFunction (fromJust $ elemIndex f fs) f fs) fs
