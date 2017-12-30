{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Syntax
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Data.Char(isSpace)
import qualified Data.Map as Map
import qualified Text.ParserCombinators.Parsec.Token as Token


lexer         = Token.makeTokenParser languageDef

identifier    = Token.identifier    lexer -- parses an identifier
reserved      = Token.reserved      lexer -- parses a reserved name
reservedOp    = Token.reservedOp    lexer -- parses an operator
parens        = Token.parens        lexer -- parses surrounding parenthesis:
braces        = Token.braces        lexer
float         = Token.float         lexer -- parses a floating point value
stringLiteral = Token.stringLiteral lexer --parses a literal string


int :: Parser Int
int = fromIntegral <$> (Token.integer lexer)


double :: Parser Double
double = try float <|> fromIntegral <$> int


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


oneOfKeys :: Map.Map String a -> Parser a
oneOfKeys m = ((Map.!) m) <$> (choice . map string . Map.keys $ m)


unaryOperation :: Parser UnaryOperation
unaryOperation = oneOfKeys unaryOperations


binaryOperation :: Parser BinaryOperation
binaryOperation = oneOfKeys binaryOperations


numVar :: Parser NumExpression
numVar = NumVar <$> double


unaryOperationExpression :: Parser NumExpression
unaryOperationExpression = do
    op <- unaryOperation
    spaces
    ex <- numExpression
    return $ UnaryExpression op ex


numExpression :: Parser NumExpression
numExpression = numVar
            <|> unaryOperationExpression