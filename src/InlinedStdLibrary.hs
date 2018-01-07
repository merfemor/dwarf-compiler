module InlinedStdLibrary where 

import Data.List(findIndex)
import Syntax.Abstract (Type, Type(Double), Type(String), Type(Int), Var, Var(Var))
import Syntax.Translatable as T
import Syntax.ByteCode     as BC

standardFunctions :: [T.Function]
standardFunctions = [ T.Function Nothing       "print"    [] [Var String ""] Nothing []
                    , T.Function Nothing       "printn"   [] [Var Double ""] Nothing []
                    , T.Function (Just Int)    "dtoi"     [] [Var Double ""] Nothing []
                    ]


bodiesOfStandardFunctions :: [[BCCommand]]
bodiesOfStandardFunctions = [[SPRINT, RETURN], [DPRINT, RETURN], [D2I, RETURN]]

                    
isStandardFunction :: String -> Bool
isStandardFunction n = Nothing /= findIndex (\f -> T.funcName f == n) standardFunctions
