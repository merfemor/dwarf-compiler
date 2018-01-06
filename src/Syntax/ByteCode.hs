module Syntax.ByteCode where

import Syntax.Translatable(Id)

data BCCommand = ILOAD Int | DLOAD Double | LOADS Id
               | DADD | IADD | DSUB | ISUB
               | DMUL | IMUL | DDIV | IDIV | IMOD
               | DNEG | INEG
               | IPRINT | DPRINT | SPRINT
               | I2D | D2I | S2I
               | SWAP | POP
               | LOADVAR Id | LOADSVAR Id | LOADCTXDVAR Id Id
               | STOREVAR Id | STORECTXVAR Id Id
               | DCMP | ICMP | JA Int | IFICMPNE Int
               | IFICMPE Int | IFICMPG Int | IFICMPGE Int
               | IFICMPL Int | IFICMPLE Int | DUMP
               | CALL Id | RETURN | BREAK | STOP deriving Show 
