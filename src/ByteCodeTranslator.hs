module ByteCodeTranslator(toByteCode) where

import Syntax.Translatable as T
import Syntax.ByteCode     as BC
import Data.Maybe(fromJust)
import Data.List(elemIndex)

-- TODO: set std functions body in the end of translating

addFunctionNamesToStringPool :: TranslatableProgramTree -> [String]
addFunctionNamesToStringPool (sp,fp) = sp ++ map T.funcName fp


translateStatement :: TranslatableProgramTree -> Statement -> [BCCommand]
translateStatement = undefined


translateFunction :: TranslatableProgramTree -> T.Function -> BC.Function
translateFunction t@(sp,fp) (T.Function _ fn lvs args _ ss) = 
    let fnid = fromJust $ elemIndex fn sp
        ss' = concatMap (translateStatement t) ss
    in BC.Function fnid (lvs ++ args) args ss'  -- TODO: maybe ss' + [RETURN]
                         


toByteCode :: TranslatableProgramTree -> ByteCodeProgramTree
toByteCode tpt@(_,fp) = 
    let sp' = addFunctionNamesToStringPool tpt 
        fp' = map (translateFunction (sp',fp)) fp
    in (sp', fp')
