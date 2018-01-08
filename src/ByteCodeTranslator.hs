module ByteCodeTranslator(toByteCode) where

import Syntax.Abstract ( Type(..)
                       , UnaryOperation(..)
                       , BinaryOperation(..)
                       , ExType(..)
                       , varType)
import Syntax.Translatable as T
import Syntax.ByteCode     as BC
import TypeChecker(expressionType)
import Data.Maybe(fromJust)
import Data.List(elemIndex, findIndex)
import InlinedStdLibrary


getFunctionName :: [T.Function] -> T.Function -> String
getFunctionName fs f = case outerFunctionId f of
                                  Nothing  -> T.funcName f
                                  Just oid -> getFunctionName fs (fs !! oid) ++ "." ++ T.funcName f
        

addFunctionNamesToStringPool :: TranslatableProgramTree -> [String]
addFunctionNamesToStringPool (sp,fp) = sp ++ map (getFunctionName fp) fp


translateVarId :: [T.Function] -> VariableId -> Id
translateVarId _  (VariableId _   vid True)  = vid
translateVarId fs (VariableId fid vid False) = vid + length (T.arguments (fs !! fid))


translateUnaryOperation :: ExType -> UnaryOperation -> [BCCommand]
translateUnaryOperation (StdType (Just Int))    Neg = [INEG]
translateUnaryOperation (StdType (Just Double)) Neg = [DNEG]
translateUnaryOperation Boolean Not = 
    [ LOAD_i 0
    , IFICMPE 2
    , LOAD_i 0
    , JA 1
    , LOAD_i 1]
translateUnaryOperation t o = error $ "can't generate bytecode of operation " ++ show o ++ " for type " ++ show t


intBoolOpCode :: (Int -> BCCommand) -> [BCCommand]
intBoolOpCode c = c 2 : [LOAD_i 0, JA 1, LOAD_i 1]


doubleBoolOpCode :: (Int -> BCCommand) -> [BCCommand]
doubleBoolOpCode c = [DCMP, LOAD_i 0, SWAP] ++ intBoolOpCode c


translateNumBinOp :: Type -> BinaryOperation -> [BCCommand]
translateNumBinOp Int    Sum = [IADD]
translateNumBinOp Double Sum = [DADD]
translateNumBinOp Int    Sub = [ISUB]
translateNumBinOp Double Sub = [DSUB]
translateNumBinOp Int    Mul = [IMUL]
translateNumBinOp Double Mul = [DMUL]
translateNumBinOp Int    Div = [IDIV]
translateNumBinOp Double Div = [DDIV]
translateNumBinOp Int L       = intBoolOpCode IFICMPL
translateNumBinOp Int LE      = intBoolOpCode IFICMPLE
translateNumBinOp Int G       = intBoolOpCode IFICMPG
translateNumBinOp Int GE      = intBoolOpCode IFICMPGE
translateNumBinOp Int Eq      = intBoolOpCode IFICMPE
translateNumBinOp Int NotE    = intBoolOpCode IFICMPNE
translateNumBinOp Double L    = doubleBoolOpCode IFICMPL
translateNumBinOp Double LE   = doubleBoolOpCode IFICMPLE
translateNumBinOp Double G    = doubleBoolOpCode IFICMPG
translateNumBinOp Double GE   = doubleBoolOpCode IFICMPGE
translateNumBinOp Double Eq   = doubleBoolOpCode IFICMPE
translateNumBinOp Double NotE = doubleBoolOpCode IFICMPNE
translateNumBinOp t o = error $ "can't generate bytecode of operation " ++ show o ++ " for type " ++ show t


translateBoolBinOp :: BinaryOperation -> [BCCommand]
translateBoolBinOp And = [IMUL]
translateBoolBinOp Or  = [ IADD, LOAD_i 0, IFICMPE 1, LOAD_i 1 ]
translateBoolBinOp c   = error $ show c ++ " is not an operation under boolean arguments"


translateFunctionCall :: [T.Function] -> Id -> [Expression] -> [BCCommand]
translateFunctionCall fs i es = 
    let cvrts = getArgumentConverts fs i es 
        es' = map (translateExpression fs) es
        es'' = map (uncurry (++)) (zip es' cvrts)
    in
    concat (reverse es'') ++
    [CALL i]


translateExpression :: [T.Function] -> Expression -> [BCCommand]
translateExpression _ (SLit i) = [LOADS i]
translateExpression _ (ILit i) = [LOAD_i i]
translateExpression _ (DLit i) = [LOAD_d i]
translateExpression fs (FCall i exs) = translateFunctionCall fs i exs 
translateExpression fs (VCall vid) = [LOADCTXVAR (funcId vid) (translateVarId fs vid)]
translateExpression fs (UnaryExpression op e) = 
    translateExpression fs e ++ 
    translateUnaryOperation (expressionType fs e) op 
translateExpression fs (BinaryExpression op ex1 ex2) =
    let ex1' = translateExpression fs ex1
        ex2' = translateExpression fs ex2
        t1 = expressionType fs ex1
        t2 = expressionType fs ex2
    in case op of
        Or  -> ex2' ++ ex1' ++ translateBoolBinOp op
        And -> ex2' ++ ex1' ++ translateBoolBinOp op
        Div -> ex2' ++ 
               (if t2 == StdType (Just Int) then [I2D] else []) ++ 
               ex1' ++ 
               (if t1 == StdType (Just Int) then [I2D] else []) ++ 
               translateNumBinOp Double Div
        _ -> let StdType (Just t1') = t1
                 StdType (Just t2') = t2 in 
                 ex2' ++ 
                 (if t2' == Int && t1' == Double then [I2D] else []) ++
                 ex1' ++
                 (if t1' == Int && t2' == Double then [I2D] else []) ++
                 translateNumBinOp (if t1' == t2' then t1' else Double) op

    
getArgumentConverts :: [T.Function] -> Id -> [Expression] -> [[BCCommand]]
getArgumentConverts fs fid es =
    let ext = map varType (T.arguments (fs !! fid))
        it = map (\(StdType (Just t)) -> t) (map (expressionType fs) es)
        cvrt (Double,Int) = [I2D]
        cvrt _            = []
    in map cvrt (zip ext it)

    
translateStatement :: T.Function -> [T.Function] -> Statement -> [BCCommand]
translateStatement _ _ (Return Nothing) = [RETURN]
translateStatement f fs (Return (Just e)) =
    let StdType t = expressionType fs e in
    translateExpression fs e ++ 
    (if t == Just Int && returnType f == Just Double then [I2D] else []) ++
    [RETURN]

translateStatement _ fs (FuncCall i es) = translateFunctionCall fs i es

translateStatement _ fs (VarAssign vid@(VariableId fid vi isA) e) = 
    let StdType (Just t) = expressionType fs e 
        v = (if isA then T.arguments else T.localVars) (fs !! fid) !! vi in
    translateExpression fs e ++ 
    (if t == Int && varType v == Double then [I2D] else []) ++
    [STORECTXVAR (funcId vid) (translateVarId fs vid)]

translateStatement fid fs (WhileLoop e ss) = 
    let ss' = concatMap (translateStatement fid fs) ss
        offset = length ss' + 1 
        e'  = translateExpression fs e
        elen = length e'
    in e' ++ [LOAD_i 0, IFICMPE offset] ++ ss' ++ [JA (-offset - 2 - elen)]
    
translateStatement fid fs (IfElse e iss ess) = 
    let e' = translateExpression fs e
        iss' = concatMap (translateStatement fid fs) iss
        ess' = concatMap (translateStatement fid fs) ess
        isslen = length iss'
    in e' ++ [LOAD_i 0, IFICMPE isslen] ++ iss' ++ ess'


translateFunction :: TranslatableProgramTree -> T.Function -> BC.Function
translateFunction (sp,fp) f@(T.Function _ fn lvs args _ ss) = 
    let ss'  = concatMap (translateStatement f fp) ss
        fn'  = getFunctionName fp f
        fnid = fromJust $ elemIndex fn' sp
        returnToStop RETURN = STOP
        returnToStop s = s
        ss'' = if fn == "main" then map returnToStop ss' else ss'
    in BC.Function fnid (args ++ lvs) (map varType args) (map STOREVAR [0..length args - 1] ++  ss'')

    
setStandardFunBodies :: [BC.Function] -> [BC.Function]
setStandardFunBodies sfs = map replaceBody (zip [0..] sfs) where
    replaceBody (i, (BC.Function n l as _)) = BC.Function n l as (bodiesOfStandardFunctions !! i)

    
replaceMainAnd0 :: ByteCodeProgramTree -> [BC.Function]
replaceMainAnd0 (sp,fp) = 
    let mainFId = fromJust $ findIndex (\f -> sp !! (BC.funcName f) == "main") fp
        headFun = head fp
        mainFun = fp !! mainFId
        fp' = update fp headFun mainFId
        fp'' = mainFun : tail fp' 
        replCom f t (CALL fun) 
            | fun == t = CALL f
            | fun == f = CALL t
        replCom f t (STORECTXVAR fun vid) 
            | fun == t = STORECTXVAR f vid
            | fun == f = STORECTXVAR t vid
        replCom f t (LOADCTXVAR fun vid) 
            | fun == t = LOADCTXVAR f vid
            | fun == f = LOADCTXVAR t vid
        replCom _ _ c = c
        replaceFcallId f t (BC.Function n l a cs) = BC.Function n l a (map (replCom f t) cs)
    in map (replaceFcallId mainFId 0) fp''
    

toByteCode :: TranslatableProgramTree -> ByteCodeProgramTree
toByteCode tpt@(_,fp) = 
    let sp' = addFunctionNamesToStringPool tpt
        fp' = map (translateFunction (sp',fp)) fp
        sfs = take (length standardFunctions) fp'
        ufs = drop (length standardFunctions) fp'
        sfs' = setStandardFunBodies sfs
        fp'' = sfs' ++ ufs
        fp''' = replaceMainAnd0 (sp',fp'')
    in (sp', fp''')
