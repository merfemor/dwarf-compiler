module Translator(abstractToTranslatable) where

import Syntax.Translatable as T
import Syntax.Abstract     as A
import Syntax.Error
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

-- TODO: test this algorithm
findLocalFunction :: [T.Function] -> Id -> Id -> String -> Maybe Id
findLocalFunction fs iid to fn = 
    let correctOutId = \f -> outerFunctionId f == Just iid in
    case findIndex (\f -> (T.funcName f == fn) && correctOutId f) (take to fs) of
         Just ffid -> Just ffid
         Nothing   -> case outerFunctionId (fs !! iid) of
                           Nothing   -> Nothing
                           Just ofid -> findLocalFunction fs ofid to fn


findFunction :: [T.Function] -> Id -> String -> Maybe Id
findFunction fs fid fn = 
    let global = \f -> outerFunctionId f == Nothing in
    case findIndex (\f -> global f && (T.funcName f == fn)) fs of
         Just ffid -> Just ffid
         Nothing   -> findLocalFunction fs fid (length fs) fn


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

translateExpression fid (A.VCall v) s@(_, fp) =
    case findVariable fp fid v of
         Nothing  -> Left $ UndefinedVariable v
         Just vid -> Right (T.VCall vid, s)

translateExpression fid (A.FCall (FunctionCall fn exs)) s@(_, fp) = 
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


translateStatement :: Id -> TreeTransaltor A.Statement T.Statement
translateStatement = undefined


translatateFunction :: Id -> TreeTransaltor A.Function T.Function
translatateFunction = undefined


makeGlobalFunctionSignatures :: TreeTransaltor AbstractProgramTree ()
makeGlobalFunctionSignatures [] s                              = Right ((), s)
makeGlobalFunctionSignatures (af@(A.Function t fn _ _):fs) (sp, fp) = 
    case find (\f -> A.funcName f == fn) fs of
         Just _  -> Left $ DuplicateFunctionDefinition af
         Nothing -> let tf = T.Function t fn [] [] Nothing [] in
                    makeGlobalFunctionSignatures fs (sp, fp ++ [tf])


abstractToTranslatable :: AbstractProgramTree -> Either CompilationError TranslatableProgramTree
abstractToTranslatable at = 
    case makeGlobalFunctionSignatures at ([],[]) of
         Left err      -> Left err
         Right (_, tt) -> undefined
