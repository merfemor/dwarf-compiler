module ByteCodeTranslator(toByteCode) where

import Syntax.Abstract(UnaryOperation, BinaryOperation)
import Syntax.Translatable as T
import Syntax.ByteCode     as BC
import Data.Maybe(fromJust)
import Data.List(elemIndex)

-- TODO: set std functions body in the end of translating

addFunctionNamesToStringPool :: TranslatableProgramTree -> [String]
addFunctionNamesToStringPool (sp,fp) = sp ++ map T.funcName fp


translateVarId :: [T.Function] -> VariableId -> Id
translateVarId _  (VariableId _   vid True)  = vid
translateVarId fs (VariableId fid vid False) = vid + length (T.arguments (fs !! fid))


translateUnaryOperation :: UnaryOperation -> [BCCommand]
translateUnaryOperation = undefined


translateBinaryOperation :: BinaryOperation -> [BCCommand]
translateBinaryOperation = undefined


translateExpression :: [T.Function] -> Expression -> [BCCommand]
translateExpression _ (SLit i) = [LOADSVAR i]
translateExpression _ (ILit i) = [LOAD_i i]
translateExpression _ (DLit i) = [LOAD_d i]
translateExpression fs (FCall i exs) = (concatMap (translateExpression fs) exs) ++ [CALL i]
translateExpression fs (VCall vid) = [LOADCTXDVAR (funcId vid) (translateVarId fs vid)]
translateExpression fs (UnaryExpression op ex) = translateExpression fs ex ++ translateUnaryOperation op
translateExpression fs (BinaryExpression op ex1 ex2) = translateExpression fs ex1 ++ translateExpression fs ex2 ++ translateBinaryOperation op


translateStatement :: [T.Function] -> Statement -> [BCCommand]
translateStatement _ (Return Nothing) = [RETURN]
translateStatement fs (Return (Just e)) = translateExpression fs e ++ [RETURN]
translateStatement fs (FuncCall i es) = concatMap (translateExpression fs) es ++ [CALL i]
{-
                 VarAssign VariableId  Expression
               | WhileLoop Expression [Statement]
               | IfElse Expression [Statement] [Statement]
-}

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
