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

    
translateStatement :: Id -> TypeTranslator Statement
translateStatement fid s@(Return Nothing) fs = let f = fs !! fid in
    case returnType f of
         Nothing -> Right s
         et      -> Left $ ReturnTypeMismatch (funcName f) (TypeMismatch et Nothing)
{-

                 VarAssign VariableId  Expression
               | WhileLoop Expression [Statement]
               | IfElse Expression [Statement] [Statement]
               | Return (Maybe Expression)
               | FuncCall Id [Expression]
-}


translateFunction :: Id -> TypeTranslator Function
translateFunction fid (Function a b c d e ss) fs = do
    ss' <- fromEitherList $ map (\s -> translateStatement fid s fs) ss
    return $  Function a b c d e ss'
        

translateFunctions :: [Function] -> Either CompilationError [Function]
translateFunctions fs = fromEitherList $
    map (\f -> translateFunction (fromJust $ elemIndex f fs) f fs) fs
