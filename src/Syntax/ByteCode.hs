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
