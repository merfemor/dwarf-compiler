module Translator(abstractToTranslatable) where

import Syntax.Translatable as T
import Syntax.Abstract     as A
import Syntax.Error
import Data.List

type TreeTransaltor a b = a -> TranslatableProgramTree -> Either CompilationError (b, TranslatableProgramTree)


insertAndGetId :: [a] -> a -> (Int, [a])
insertAndGetId l e = (length l, l ++ [e])


update :: [a] -> a -> Int -> [a]
update xs e i = take i xs ++ [e] ++ drop (i + 1) xs


setFunctionBody :: [T.Function] -> Id -> [T.Statement] -> [T.Function]
setFunctionBody fs fid ss = 
    let T.Function a b c d e _ = fs !! fid in
        update fs (T.Function a b c d e ss) fid

        
translateMany :: TreeTransaltor a b -> TreeTransaltor [a] [b]
translateMany f []       s = Right ([], s)
translateMany f (x:xs) s = do
    (x', s') <- f x s 
    (xs', s'') <- translateMany f xs s'
    return $ (x' : xs', s'')


findVariable :: [T.Function] -> Id -> String -> Maybe VariableId
findVariable fs fid vn = 
    let isV = \v -> varName v == vn
        f   = fs !! fid in
    case findIndex isV (T.arguments f) of
        Just vid -> Just $ VariableId fid vid True
        Nothing  -> case findIndex isV (localVars f) of
                         Just vid -> Just $ VariableId fid vid False
                         Nothing  -> do
                             ofid <- outerFunctionId f
                             findVariable fs ofid vn

-- TODO: test this algorithm
findLocalFunction :: [T.Function] -> Id -> Id -> String -> Maybe Id
findLocalFunction fs iid to fn = 
    let correctOutId = \f -> outerFunctionId f == Just iid in
    case findIndex (\f -> (T.funcName f == fn) && correctOutId f) (take to fs) of
         Just ffid -> Just ffid
         Nothing   -> do
             ofid <- outerFunctionId (fs !! iid)
             findLocalFunction fs ofid to fn


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

translateExpression fid (A.UnaryExpression op e) s = do
    (e', s') <- translateExpression fid e s
    return (T.UnaryExpression op e', s')

translateExpression fid (A.BinaryExpression op e1 e2) s = do
    (e1', s') <- translateExpression fid e1 s
    (e2', s'') <- translateExpression fid e2 s'
    return (T.BinaryExpression op e1' e2', s'')

translateExpression fid (A.VCall v) s@(_, fp) =
    case findVariable fp fid v of
         Nothing  -> Left $ UndefinedVariable v
         Just vid -> Right (T.VCall vid, s)

translateExpression fid (A.FCall (FunctionCall fn exs)) s@(_, fp) = 
    case findFunction fp fid fn of 
         Nothing   -> Left $ UndefinedFunction fn
         Just ffid -> do
             (exs', s') <- translateMany (translateExpression fid) exs s 
             return (T.FCall ffid exs', s')


translateStatement :: Id -> TreeTransaltor A.Statement T.Statement
translateStatement fid (A.FuncCall (FunctionCall n es)) s@(_,fp) = 
    case findFunction fp fid n of
         Nothing   -> Left $ UndefinedFunction n
         Just ffid -> do
             (es', s') <- translateMany (translateExpression fid) es s
             return (T.FuncCall ffid es', s')

translateStatement fid (A.WhileLoop ex ss) s = do
    (ex', s') <- translateExpression fid ex s
    (ss', s'') <- translateMany (translateStatement fid) ss s'
    return (T.WhileLoop ex' ss', s'')
    
translateStatement fid (A.IfElse ex iss ess) s = do
    (ex', s') <- translateExpression fid ex s
    (iss', s'') <- translateMany (translateStatement fid) iss s'
    (ess', s''') <- translateMany (translateStatement fid) ess s''
    return (T.IfElse ex' iss' ess', s''')
    
translateStatement _ (A.Return Nothing) s = Right (T.Return Nothing, s)
translateStatement fid (A.Return (Just ex)) s = do
    (ex', s') <- translateExpression fid ex s
    return (T.Return (Just ex'), s')
-- TODO: add VarDef Var Expression, VarAssign String Expression, FuncDef Function parsing

translateGlobalFunction :: TreeTransaltor A.Function ()
translateGlobalFunction (A.Function t n args ss) s@(_,fp) = 
    let f = T.Function t n [] args (Just fid) []
        Just fid = findIndex (\f -> T.funcName f == n) fp 
    in do
        (ss', (sp,fp')) <- translateMany (translateStatement fid) ss s
        return ((), (sp, setFunctionBody fp' fid ss'))


translateFunction :: Id -> TreeTransaltor A.Function ()
translateFunction fid (A.Function t n args ss) s@(sp,fp) = 
    let f = T.Function t n [] args (Just fid) []
        (nfid,fp') = insertAndGetId fp f in do
    (ss', s') <- translateMany (translateStatement nfid) ss (sp, fp')
    return undefined

         
makeGlobalFunctionSignatures :: TreeTransaltor AbstractProgramTree ()
makeGlobalFunctionSignatures [] s                              = Right ((), s)
makeGlobalFunctionSignatures (af@(A.Function t fn _ _):fs) (sp, fp) = 
    case find (\f -> A.funcName f == fn) fs of
         Just _  -> Left $ DuplicateFunctionDefinition af
         Nothing -> let tf = T.Function t fn [] [] Nothing [] in
                    makeGlobalFunctionSignatures fs (sp, fp ++ [tf])


abstractToTranslatable :: AbstractProgramTree -> Either CompilationError TranslatableProgramTree
abstractToTranslatable t = do
    (_, tt) <- makeGlobalFunctionSignatures t ([], standartFunctions)
    (_, tt') <- translateMany translateGlobalFunction t tt
    return tt'
