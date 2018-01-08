module Printer(putProgram) where

import Syntax.ByteCode
import Data.Binary.Put
import Data.Binary
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS


putCommand :: BCCommand -> Put
putCommand bcc = case bcc of
    LOAD_d i   -> putWord8 1 >> putByteString (BS.concat (L.toChunks (encode i)))
    LOAD_i i   -> putWord8 1 >> putWord64le (fromIntegral i)
    LOADS i    -> putWord8 2 >> putWord64le (fromIntegral i)
    DADD       -> putWord8 3
    IADD       -> putWord8 4
    DSUB       -> putWord8 5
    ISUB       -> putWord8 6
    DMUL       -> putWord8 7
    IMUL       -> putWord8 8
    DDIV       -> putWord8 9
    IDIV       -> putWord8 10
    IMOD       -> putWord8 11
    DNEG       -> putWord8 12
    INEG       -> putWord8 13
    IPRINT     -> putWord8 14
    DPRINT     -> putWord8 15
    SPRINT     -> putWord8 16
    I2D        -> putWord8 17
    D2I        -> putWord8 18
    S2I        -> putWord8 19
    SWAP       -> putWord8 20
    POP        -> putWord8 21
    LOADVAR i  -> putWord8 22 >> putWord32le (fromIntegral i)
    LOADSVAR i -> putWord8 23 >> putWord32le (fromIntegral i)
    LOADCTXVAR fi vi  -> putWord8 24 >> putWord64le (fromIntegral fi) >> putWord32le (fromIntegral vi)
    STOREVAR i -> putWord8 25 >> putWord32le (fromIntegral i)
    STORECTXVAR fi vi -> putWord8 26 >> putWord64le (fromIntegral fi) >> putWord32le (fromIntegral vi)
    DCMP       -> putWord8 27
    ICMP       -> putWord8 28
    JA i       -> putWord8 29 >> putWord16le (fromIntegral i)
    IFICMPNE i -> putWord8 30 >> putWord16le (fromIntegral i)
    IFICMPE  i -> putWord8 31 >> putWord16le (fromIntegral i)
    IFICMPG  i -> putWord8 32 >> putWord16le (fromIntegral i)
    IFICMPGE i -> putWord8 33 >> putWord16le (fromIntegral i)
    IFICMPL  i -> putWord8 34 >> putWord16le (fromIntegral i)
    IFICMPLE i -> putWord8 35 >> putWord16le (fromIntegral i)
    DUMP       -> putWord8 36
    STOP       -> putWord8 37
    CALL i     -> putWord8 38 >> putWord64le (fromIntegral i)
    RETURN     -> putWord8 39
    
    
putConstPool :: [String] -> Put
putConstPool cp = let cp' = map (++ "\0") cp
                      cpsz = sum (map length cp') in do
                          putWord64le $ fromIntegral cpsz
                          foldl1 (>>) (map putStringUtf8 cp') 

                          
putFunc :: Function -> Put
putFunc (Function n l a b) = do
    putWord64le (fromIntegral n)
    putWord64le (fromIntegral (length l))
    putWord64le 0 -- not exported and not native
    putWord64le (fromIntegral (length a))
    putWord16le undefined -- arg types
    putWord64le undefined -- byte code size
    foldl1 (>>) (map putCommand b)


putProgram :: ByteCodeProgramTree -> Put
putProgram (sp,fp) = do
    putWord16le 0xBABA
    putWord64le 1 -- version     
    putConstPool sp
    putWord64le . fromIntegral . length $ fs
    foldl1 (>>) (map putFunc fs)
