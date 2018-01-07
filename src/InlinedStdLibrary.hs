module InlinedStdLibrary where 

import Data.List(findIndex)
import Syntax.Abstract (Type, Type(Double), Type(String), Type(Int), Var, Var(Var))
import Syntax.Translatable as T
import Syntax.ByteCode     as BC

standartFunctions :: [T.Function]
standartFunctions = [ T.Function Nothing       "print"    [] [Var String ""] Nothing []
                    , T.Function Nothing       "printn"   [] [Var Double ""] Nothing []
                    , T.Function (Just Int)    "dtoi"     [] [Var Double ""] Nothing []
                    ]


stdFunctionsCode :: [[BCCommand]]
stdFunctionsCode = [[], [], []] -- TODO: write std functions

                    
isStandartFunction :: String -> Bool
isStandartFunction n = Nothing /= findIndex (\f -> T.funcName f == n) standartFunctions
