{-# LANGUAGE FlexibleContexts #-}
module Parser (programTree) where

import Syntax
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Text.Parsec.Char
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
commaSep      = Token.commaSep      lexer
semi          = Token.semi          lexer -- parses ;
float         = Token.float         lexer -- parses a floating point value
stringLiteral = Token.stringLiteral lexer -- parses a literal string


int :: Parser Int
int = (read <$> many1 digit) <* spaces

double :: Parser Double
double = try float <|> fromIntegral <$> int

tabsSpaces :: Parser ()
tabsSpaces = skipMany $ oneOf "\t "

mapValueBetweenSpaces m v = (spaces *> string (lookupR v m) <* spaces)

oneOfKeys :: Map.Map String a -> Parser a
oneOfKeys m = ((Map.!) m) <$> (choice . map string . Map.keys $ m)

functionCall :: Parser FunctionCall
functionCall = FunctionCall <$> identifier <*> parens sepExpressions

unOp op = Prefix $ UnaryExpression op <$ (mapValueBetweenSpaces unaryOperations op)

binOp op = Infix (BinaryExpression op <$ (mapValueBetweenSpaces binaryOperations op)) AssocLeft

operations = [[unOp Not, unOp Neg],
              [binOp Mul, binOp Div],
              [binOp Sum, binOp Sub],
              [binOp L, binOp G, binOp GE, binOp LE],
              [binOp Eq, binOp NotE],
              [binOp And],
              [binOp Or]]

subExpression :: Parser Expression
subExpression = parens expression
            <|> try (FCall  <$> functionCall)
            <|> VCall  <$> identifier
            <|> NumVar <$> double
            <|> SVar   <$> stringLiteral

expression :: Parser Expression
expression = buildExpressionParser operations subExpression

sepExpressions :: Parser [Expression]
sepExpressions = commaSep expression

builtInType :: Parser Type
builtInType = oneOfKeys builtInTypes

varDefinition :: Parser Var
varDefinition = Var <$> builtInType <* spaces <*> identifier <* char '=' <* spaces <*> expression

varAssignment :: Parser Statement
varAssignment = VarAssign <$> identifier <* char '=' <* spaces <*> expression

-- spacesEol :: Parser ()
-- spacesEol = tabsSpaces <* endOfLine *> tabsSpaces

lineSeparator :: Parser String
lineSeparator = semi

ifElse :: Parser Statement
ifElse = do
    string "if"
    spaces
    e <- parens expression
    sl <- braces statementList
    esl <- option [] (string "else" *> spaces *> braces statementList)
    return $ IfElse e sl esl

whileLoop :: Parser Statement
whileLoop = do
    string "while"
    spaces
    e <- parens expression
    sl <- braces statementList
    return $ WhileLoop e sl

statement :: Parser Statement
statement = VarDef <$> varDefinition
        <|> varAssignment
        <|> FuncDef <$> function
        <|> Return <$> (string "return" *> spaces *> optionMaybe expression)
        <|> ifElse
        <|> whileLoop

statementList :: Parser [Statement]
statementList = endBy1 statement lineSeparator -- TODO: make separation of statements by endOfLine

voidableType :: Parser (Maybe Type)
voidableType = Just <$> builtInType <|> Nothing <$ string "void"

function :: Parser Function
function = do
    t <- voidableType
    spaces
    n <- identifier
    args <- parens $ commaSep $ (,) <$> builtInType <* spaces <*> identifier
    fb <- braces statementList
    return $ Function t n args fb

programTree :: Parser ProgramTree
programTree = many1 function
