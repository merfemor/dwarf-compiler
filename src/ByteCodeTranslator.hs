module ByteCodeTranslator(toByteCode) where

import Syntax.Abstract ( Type, Type(Int), Type(Double), Type(String)
                       , UnaryOperation, UnaryOperation(Neg), UnaryOperation(Not)
                       , BinaryOperation, BinaryOperation(And), BinaryOperation(Or)
                       , BinaryOperation(Eq), BinaryOperation(G), BinaryOperation(L), BinaryOperation(GE), BinaryOperation(LE), BinaryOperation(NotE)
                       , BinaryOperation(Sum), BinaryOperation(Sub), BinaryOperation(Mul), BinaryOperation(Div)
                       , ExType, ExType(StdType))
import Syntax.Translatable as T
import Syntax.ByteCode     as BC
import TypeChecker(expressionType)
import Data.Maybe(fromJust)
import Data.List(elemIndex)

-- TODO: set std functions body in the end of translating

addFunctionNamesToStringPool :: TranslatableProgramTree -> [String]
addFunctionNamesToStringPool (sp,fp) = sp ++ map T.funcName fp


translateVarId :: [T.Function] -> VariableId -> Id
translateVarId _  (VariableId _   vid True)  = vid
translateVarId fs (VariableId fid vid False) = vid + length (T.arguments (fs !! fid))


translateUnaryOperation :: ExType -> UnaryOperation -> [BCCommand]
translateUnaryOperation (StdType (Just Int))    Neg = [INEG]
translateUnaryOperation (StdType (Just Double)) Neg = [DNEG]
translateUnaryOperation _ Not = [LOAD_i 1, ISUB] -- TODO: if TOS = 0 then TOS became -1: is it normal?


translateBinaryOperation :: ExType -> BinaryOperation -> [BCCommand]
translateBinaryOperation (StdType (Just Int))    Sum = [IADD]
translateBinaryOperation (StdType (Just Double)) Sum = [DADD]
translateBinaryOperation (StdType (Just Int))    Sub = [ISUB]
translateBinaryOperation (StdType (Just Double)) Sub = [DSUB]
translateBinaryOperation (StdType (Just Int))    Mul = [IMUL]
translateBinaryOperation (StdType (Just Double)) Mul = [DMUL]
translateBinaryOperation (StdType (Just Int))    Div = [IDIV]
translateBinaryOperation (StdType (Just Double)) Div = [DDIV]
translateBinaryOperation _ And = [IMUL]
translateBinaryOperation _ Or  = [IADD] -- TODO: true is anything not equal to 0? 1 + 1 = 2
translateBinaryOperation t o = error $ "can't generate bytecode of operation " ++ show o ++ " for type " ++ show t



translateExpression :: [T.Function] -> Expression -> [BCCommand]
translateExpression _ (SLit i) = [LOADSVAR i]
translateExpression _ (ILit i) = [LOAD_i i]
translateExpression _ (DLit i) = [LOAD_d i]
translateExpression fs (FCall i exs) = (concatMap (translateExpression fs) exs) ++ [CALL i]
translateExpression fs (VCall vid) = [LOADCTXDVAR (funcId vid) (translateVarId fs vid)]
translateExpression fs (UnaryExpression op e) = 
    translateExpression fs e ++ 
    translateUnaryOperation (expressionType fs e) op 
translateExpression fs e@(BinaryExpression op ex1 ex2) = 
    let t1 = expressionType fs ex1
        t2 = expressionType fs ex2
        t  = expressionType fs e in
    translateExpression fs ex1 ++ 
    (if t1 == StdType (Just Int) && t2 == StdType (Just Double) then [I2D] else []) ++
    translateExpression fs ex2 ++ 
    (if t1 == StdType (Just Double) && t2 == StdType (Just Int) then [I2D] else []) ++
    translateBinaryOperation t op


translateStatement :: [T.Function] -> Statement -> [BCCommand]
translateStatement _ (Return Nothing) = [RETURN]
translateStatement fs (Return (Just e)) = translateExpression fs e ++ [RETURN]
translateStatement fs (FuncCall i es) = concatMap (translateExpression fs) es ++ [CALL i]
translateStatement fs (VarAssign vid e) = translateExpression fs e ++ [STORECTXVAR (funcId vid) (translateVarId fs vid)]
{- | WhileLoop Expression [Statement]
   | IfElse Expression [Statement] [Statement] -}

-- TODO: store args in local vars
translateFunction :: TranslatableProgramTree -> T.Function -> BC.Function
translateFunction (sp,fp) (T.Function _ fn lvs args _ ss) = 
    let fnid = fromJust $ elemIndex fn sp
        ss'  = concatMap (translateStatement fp) ss
        returnToStop RETURN = STOP
        returnToStop s = s
        ss'' = if fn == "main" then map returnToStop ss' else ss'
    in BC.Function fnid (args ++ lvs) args ss''
                         


toByteCode :: TranslatableProgramTree -> ByteCodeProgramTree
toByteCode tpt@(_,fp) = 
    let sp' = addFunctionNamesToStringPool tpt 
        fp' = map (translateFunction (sp',fp)) fp
    in (sp', fp')
