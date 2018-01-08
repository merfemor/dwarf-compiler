module ByteCodeTranslator(toByteCode) where

import Syntax.Abstract ( Type, Type(Int), Type(Double), Type(String)
                       , UnaryOperation, UnaryOperation(Neg), UnaryOperation(Not)
                       , BinaryOperation, BinaryOperation(And), BinaryOperation(Or)
                       , BinaryOperation(Eq), BinaryOperation(G), BinaryOperation(L), BinaryOperation(GE), BinaryOperation(LE), BinaryOperation(NotE)
                       , BinaryOperation(Sum), BinaryOperation(Sub), BinaryOperation(Mul), BinaryOperation(Div)
                       , ExType, ExType(StdType), ExType(Boolean)
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
translateUnaryOperation Boolean Not = [LOAD_i 1, IADD] -- FIXME: ! (! False) = True
translateUnaryOperation t o = error $ "can't generate bytecode of operation " ++ show o ++ " for type " ++ show t


translateBinaryOperation :: ExType -> BinaryOperation -> [BCCommand]
translateBinaryOperation (StdType (Just Int))    Sum = [IADD]
translateBinaryOperation (StdType (Just Double)) Sum = [DADD]
translateBinaryOperation (StdType (Just Int))    Sub = [ISUB]
translateBinaryOperation (StdType (Just Double)) Sub = [DSUB]
translateBinaryOperation (StdType (Just Int))    Mul = [IMUL]
translateBinaryOperation (StdType (Just Double)) Mul = [DMUL]
translateBinaryOperation (StdType (Just Int))    Div = [IDIV]
translateBinaryOperation (StdType (Just Double)) Div = [DDIV]
translateBinaryOperation Boolean And = [IMUL]
translateBinaryOperation Boolean Or = 
    [ IADD
    , LOAD_i 0
    , IFICMPE 2
    , POP
    , LOAD_i 1 ]
translateBinaryOperation (StdType (Just Int)) L = 
    [ IFICMPL 3
    , LOAD_i 0
    , JA 1
    , LOAD_i 1 ]
    
translateBinaryOperation t o = error $ "can't generate bytecode of operation " ++ show o ++ " for type " ++ show t



translateExpression :: [T.Function] -> Expression -> [BCCommand]
translateExpression _ (SLit i) = [LOADS i]
translateExpression _ (ILit i) = [LOAD_i i]
translateExpression _ (DLit i) = [LOAD_d i]
translateExpression fs (FCall i exs) = (concatMap (translateExpression fs) exs) ++ [CALL i]
translateExpression fs (VCall vid) = [LOADCTXVAR (funcId vid) (translateVarId fs vid)]
translateExpression fs (UnaryExpression op e) = 
    translateExpression fs e ++ 
    translateUnaryOperation (expressionType fs e) op 
translateExpression fs (BinaryExpression op ex1 ex2) = 
    let t1 = expressionType fs ex1
        t2 = expressionType fs ex2
        t  = if t1 == t2 then t1 else StdType $ Just $ Double in
    translateExpression fs ex1 ++ 
    (if t1 == StdType (Just Int) && t2 == StdType (Just Double) then [I2D] else []) ++
    translateExpression fs ex2 ++ 
    (if t1 == StdType (Just Double) && t2 == StdType (Just Int) then [I2D] else []) ++
    translateBinaryOperation t op

    
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
    (if returnType f == Just Double && t == Just Int then [I2D] else []) ++
    [RETURN]
translateStatement _ fs (FuncCall i es) = 
    let cvrts = getArgumentConverts fs i es 
        es' = map (translateExpression fs) es
        es'' = map (uncurry (++)) (zip es' cvrts)
    in
    concat es'' ++
    [CALL i]

translateStatement _ fs (VarAssign vid e) = 
    translateExpression fs e ++ 
    [STORECTXVAR (funcId vid) (translateVarId fs vid)] -- convert types

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
