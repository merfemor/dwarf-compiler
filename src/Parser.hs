module Parser where

import Syntax
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Data.Char(isSpace)
import qualified Text.ParserCombinators.Parsec.Token as Token

lexer         = Token.makeTokenParser languageDef

identifier    = Token.identifier    lexer -- parses an identifier
reserved      = Token.reserved      lexer -- parses a reserved name
reservedOp    = Token.reservedOp    lexer -- parses an operator
parens        = Token.parens        lexer -- parses surrounding parenthesis:
braces        = Token.braces        lexer
integer       = Token.integer       lexer -- parses an integer
float         = Token.float         lexer -- parses a floating point value
stringLiteral = Token.stringLiteral lexer --parses a literal string


double :: Parser Double
double = try float <|> fromIntegral <$> integer

-- skips tabs and spaces
tabsSpaces :: Parser ()
tabsSpaces = skipMany $ oneOf "\t "


varDefine :: Parser String
varDefine = do
   spaces
   id <- identifier
   spaces
   char '='
   spaces
   return id


iVarParser :: Parser NamedVar
iVarParser = do
    reserved "int"
    id <- varDefine
    val <- integer
    return $ NamedVar id (IVar $ fromInteger val)


dVarParser :: Parser NamedVar
dVarParser = do
    reserved "double"
    id <- varDefine
    val <- double
    return $ NamedVar id (DVar $ val)


sVarParser :: Parser NamedVar
sVarParser = do
    reserved "string"
    id <- varDefine
    val <- stringLiteral
    return $ NamedVar id (SVar $ val)