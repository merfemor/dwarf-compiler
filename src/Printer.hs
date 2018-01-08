module Printer(putProgram) where

import Syntax.ByteCode
import Data.Binary.Put


putProgram :: ByteCodeProgramTree -> Put
putProgram (sp,fp) = do
    undefined
