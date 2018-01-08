module TypeChecker(checkFunctions, expressionType) where

import Syntax.Translatable
import Syntax.Abstract ( Type(..)
                       , varType
                       , isUnaryBoolean
                       , isBinaryBoolean
                       , BinaryOperation(Div)
                       , ExType(..))
import Syntax.Error
import Data.List(elemIndex)
import Data.Maybe(fromJust)


fromEitherList :: [Either a b] -> Either a [b]
fromEitherList []     = Right []
fromEitherList (e:es) = do
    b <- e
    bs <- fromEitherList es
    return (b:bs)
    

expressionType :: [Function] -> Expression -> ExType
expressionType _ (ILit _) = StdType $ Just Int
expressionType _ (DLit _) = StdType $ Just Double
expressionType _ (SLit _) = StdType $ Just String
expressionType fs (FCall fid _) = StdType $ returnType (fs !! fid)
expressionType fs (VCall (VariableId fid vid isA)) = 
    let vars = if isA then arguments else localVars in
    StdType $ Just . varType . (!! vid) . vars . (!! fid) $ fs

expressionType fs (UnaryExpression op ex)
    | isUnaryBoolean op = Boolean 
    | otherwise         = expressionType fs ex

expressionType _  (BinaryExpression Div _   _ ) = StdType $ Just Double
expressionType fs (BinaryExpression op ex1 ex2)
    | isBinaryBoolean op = Boolean
    | otherwise          = 
        let t@(StdType (Just t1)) = expressionType fs ex1
            (StdType (Just t2)) = expressionType fs ex2 in
            if t1 == t2 then t else StdType $ Just Double

    
checkStatement :: Id -> Statement -> [Function] -> Either CompilationError ()
checkStatement _ _ _ = Right ()


checkFunction :: Function -> [Function] -> Either CompilationError ()
checkFunction f fs = let fid = fromJust $ elemIndex f fs in 
                         fromEitherList (map (\s -> checkStatement fid s fs) (functionBody f)) >> Right ()
        

checkFunctions :: [Function] -> Either CompilationError ()
checkFunctions fs = fromEitherList (map (\f -> checkFunction f fs) fs) >> Right ()
