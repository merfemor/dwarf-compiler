module Translator where

import Syntax.Translatable as T
import Syntax.Abstract     as A
import Syntax.Error
import Control.Monad.State.Lazy
import Data.List

type TreeTransaltor a b = a -> TranslatableProgramTree -> Either CompilationError (b, TranslatableProgramTree)


insertAndGetId :: [a] -> a -> (Int, [a])
insertAndGetId l e = (length l, l ++ [e])


findVariable :: [T.Function] -> Id -> String -> Maybe VariableId
findVariable fs fid vn = 
    let isV = \v -> T.varName v == vn
        f   = fs !! fid in
    case findIndex isV (T.arguments f) of
        Just vid -> Just $ VariableId fid vid True
        Nothing  -> case findIndex isV (localVars f) of
                         Just vid -> Just $ VariableId fid vid False
                         Nothing  -> case outerFunctionId f of
                                          Just ofid -> findVariable fs ofid vn
                                          Nothing   -> Nothing


findFunction :: [T.Function] -> Id -> String -> Maybe Id
findFunction fs fid fn = 
    let correctName = \f -> T.funcName f == fn
        global = \f -> outerFunctionId f == Nothing
    in
    case findIndex (\f -> global f && correctName f) fs of
         Just ffid -> Just ffid
         Nothing   -> undefined


-- context function id -> translator
translateExpression :: Id -> TreeTransaltor A.Expression T.Expression
translateExpression _ (A.ILit a) s = Right (T.ILit a, s)
translateExpression _ (A.DLit a) s = Right (T.DLit a, s)
translateExpression _ (A.SLit sl) (sp, fp) = 
    let (sid, sp') = insertAndGetId sp sl in 
        Right (T.SLit sid, (sp', fp))

translateExpression fid (A.UnaryExpression op e) s = 
    case translateExpression fid e s of
        Right (e', s') -> Right (T.UnaryExpression op e', s')
        Left err       -> Left err

translateExpression fid (A.BinaryExpression op e1 e2) s =
    case translateExpression fid e1 s of
        Left err        -> Left err
        Right (e1', s') -> case translateExpression fid e2 s' of
                                Right (e2', s'') -> Right (T.BinaryExpression op e1' e2', s'')
                                Left err         -> Left err

translateExpression fid (A.VCall v) s@(sp, fp) =
    case findVariable fp fid v of
         Nothing  -> Left $ UndefinedVariable v
         Just vid -> Right (T.VCall vid, s)

translateExpression fid (A.FCall (FunctionCall fn exs)) s@(sp, fp) = 
    case findFunction fp fid fn of 
         Nothing   -> Left $ UndefinedFunction fn
         Just ffid -> case translateExpressions fid exs s of
                           Left err         -> Left err
                           Right (exs', s') -> Right (T.FCall ffid exs', s')


translateExpressions :: Id -> TreeTransaltor [A.Expression] [T.Expression]
translateExpressions _   []       s = Right ([], s)
translateExpressions fid (ex:exs) s = 
    case translateExpression fid ex s of
         Left err        -> Left err
         Right (ex', s') -> case translateExpressions fid exs s' of
                                 Left err          -> Left err
                                 Right (exs', s'') -> Right (ex' : exs', s'')


abstractToTranslatable :: AbstractProgramTree -> TranslatableProgramTree
abstractToTranslatable = undefined
