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

whiteSpace    = Token.whiteSpace    lexer
identifier    = Token.identifier    lexer -- parses an identifier
parens        = Token.parens        lexer -- parses surrounding parenthesis:
braces        = Token.braces        lexer
commaSep      = Token.commaSep      lexer
float         = Token.float         lexer -- parses a floating point value
stringLiteral = Token.stringLiteral lexer -- parses a literal string

int :: Parser Int
int = fromInteger <$> Token.integer lexer

mapValueBetweenSpaces :: Eq a => Map.Map String a -> a -> Parser String
mapValueBetweenSpaces m v = try (whiteSpace *> string (lookupR v m) <* whiteSpace)

oneOfKeys :: Map.Map String a -> Parser a
oneOfKeys m = ((Map.!) m) <$> (choice . map string . Map.keys $ m)

functionCall :: Parser FunctionCall
functionCall = FunctionCall <$> identifier <*> parens sepExpressions <?> "function call"

unOp op = Prefix $ UnaryExpression op <$ (mapValueBetweenSpaces unaryOperations op)

binOp op = Infix (BinaryExpression op <$ (mapValueBetweenSpaces binaryOperations op)) AssocLeft

operations = [[unOp Not, unOp Neg],
              [binOp Mul, binOp Div],
              [binOp Sum, binOp Sub],
              [binOp GE, binOp LE, binOp L, binOp G],
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
builtInType = oneOfKeys builtInTypes <?> "built-in type (int, double or string)"

varDefinition :: Parser Var
varDefinition = Var <$> builtInType <* whiteSpace <*> identifier <?> "variable definition"

varAssignment :: Parser Statement
varAssignment = VarAssign <$> identifier <* char '=' <* whiteSpace <*> expression <?> "variable assignment"

ifElse :: Parser Statement
ifElse = do
    _ <- string "if"
    whiteSpace
    e <- parens expression
    sl <- braces statementList
    esl <- option [] (string "else" *> whiteSpace *> braces statementList)
    return (IfElse e sl esl) <?> "if-else block"

whileLoop :: Parser Statement
whileLoop = do
    _ <- string "while"
    whiteSpace
    e <- parens expression
    sl <- braces statementList
    return (WhileLoop e sl) <?> "while loop block"

statement :: Parser Statement
statement = try varAssignment
        <|> try (FuncDef <$> function)
        <|> (Return <$> (string "return" *> whiteSpace *> optionMaybe expression) <?> "return statement")
        <|> try ifElse
        <|> whileLoop
        <|> try (VarDef <$> varDefinition <* char '=' <* whiteSpace <*> expression)
        <|> FuncCall <$> functionCall

statementList :: Parser [Statement]
statementList = endBy1 statement whiteSpace

voidableType :: Parser (Maybe Type)
voidableType = Just <$> builtInType <|> Nothing <$ string "void"

function :: Parser Function
function = do
    t <- voidableType
    whiteSpace
    n <- identifier
    args <- parens $ commaSep $ Var <$> builtInType <* whiteSpace <*> identifier
    fb <- braces statementList
    return (Function t n args fb) <?> "function definition"

abstractProgramTree :: Parser AbstractProgramTree
abstractProgramTree = whiteSpace >> many1 function
