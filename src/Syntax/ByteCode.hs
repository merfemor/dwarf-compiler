module Syntax.ByteCode where

import Syntax.Translatable(Id)
import Syntax.Abstract(Type, Var)

data BCCommand = LOAD_i Int | LOAD_d Double | LOADS Id
               | DADD | IADD | DSUB | ISUB
               | DMUL | IMUL | DDIV | IDIV | IMOD
               | DNEG | INEG
               | IPRINT | DPRINT | SPRINT
               | I2D | D2I | S2I
               | SWAP | POP
               | LOADVAR Id | LOADSVAR Id | LOADCTXVAR Id Id
               | STOREVAR Id | STORECTXVAR Id Id
               | DCMP | ICMP | JA Int | IFICMPNE Int
               | IFICMPE Int | IFICMPG Int | IFICMPGE Int
               | IFICMPL Int | IFICMPLE Int | DUMP
               | CALL Id | RETURN | STOP deriving Show 

data Function = Function { funcName :: Id
                         , localVars :: [Var]
                         , arguments :: [Type]
                         , functionBody :: [BCCommand]
                         } deriving Show

type ByteCodeProgramTree = ([String], [Function])


commandSizeInBytes :: BCCommand -> Int
commandSizeInBytes c = 1 + case c of
    LOAD_d _        -> 8
    LOAD_i _        -> 8
    LOADS _         -> 8
    LOADVAR _       -> 4
    LOADSVAR _      -> 4
    LOADCTXVAR _ _  -> 8 + 4
    STOREVAR _      -> 4
    STORECTXVAR _ _ -> 8 + 4
    JA _            -> 2
    IFICMPNE _      -> 2
    IFICMPE _       -> 2
    IFICMPG _       -> 2
    IFICMPGE _      -> 2
    IFICMPL _       -> 2
    IFICMPLE _      -> 2
    CALL _          -> 8
    _               -> 0
