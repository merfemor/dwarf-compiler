{-# LANGUAGE FlexibleContexts #-}
module Parser (abstractProgramTree) where

import Syntax.Abstract
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import qualified Data.Map as Map
import qualified Text.ParserCombinators.Parsec.Token as Token

-- get key by value in Map
lookupR :: Eq b => b -> Map.Map c b -> c
lookupR v = fst . head . Map.assocs . (Map.filter (==v))

lexer         = Token.makeTokenParser languageDef

identifier    = Token.identifier    lexer -- parses an identifier
parens        = Token.parens        lexer -- parses surrounding parenthesis:
braces        = Token.braces        lexer
commaSep      = Token.commaSep      lexer
float         = Token.float         lexer -- parses a floating point value
stringLiteral = Token.stringLiteral lexer -- parses a literal string

int :: Parser Int
int = fromInteger <$> Token.integer lexer

mapValueBetweenSpaces :: Eq a => Map.Map String a -> a -> Parser String
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
            <|> FCall <$> try functionCall
            <|> VCall <$> identifier
            <|> DLit  <$> try float
            <|> ILit  <$> int
            <|> SLit  <$> stringLiteral

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

ifElse :: Parser Statement
ifElse = do
    _ <- string "if"
    spaces
    e <- parens expression
    sl <- braces statementList
    esl <- option [] (string "else" *> spaces *> braces statementList)
    return $ IfElse e sl esl

whileLoop :: Parser Statement
whileLoop = do
    _ <- string "while"
    spaces
    e <- parens expression
    sl <- braces statementList
    return $ WhileLoop e sl

statement :: Parser Statement
statement = try varAssignment
        <|> try (FuncDef <$> function)
        <|> Return <$> (string "return" *> spaces *> optionMaybe expression)
        <|> try ifElse
        <|> whileLoop
        <|> VarDef <$> varDefinition
        <|> FuncCall <$> functionCall

statementList :: Parser [Statement]
statementList = endBy1 statement spaces

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

abstractProgramTree :: Parser AbstractProgramTree
abstractProgramTree = many1 function
