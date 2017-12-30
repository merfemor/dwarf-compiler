{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Syntax
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Data.Char(isSpace)
import qualified Data.Map as Map
import qualified Text.ParserCombinators.Parsec.Token as Token


-- get key by value in Map
lookupR :: Eq b => b -> Map.Map c b -> c
lookupR v = fst . head . Map.assocs . (Map.filter (==v))


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


unOp op = Prefix $ do
    string (lookupR op unaryOperations)
    return $ UnaryExpression op


binOp op = Infix (do
    string (lookupR op binaryOperations)
    return $ BinaryExpression op) AssocLeft


operations = [[unOp Not, unOp Neg],
              [binOp Mul, binOp Div],
              [binOp Sum, binOp Sub],
              [binOp L, binOp G, binOp GE, binOp LE],
              [binOp Eq, binOp NotE],
              [binOp And],
              [binOp Or]]


exprParser :: Parser NumExpression
exprParser = buildExpressionParser operations numVar