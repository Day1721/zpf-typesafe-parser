module Parser where

import Data.String
import Control.Monad
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import SyntaxTree

type FancyErr = Void

type Parser = Parsec FancyErr String
type CtxParser d = Parser (d SourcePos)

formatError :: ParseErrorBundle String FancyErr -> String
formatError = errorBundlePretty


runParsing :: String -> String -> Either String (Expr SourcePos)
runParsing name code = case runParser (pProg :: CtxParser Expr) name code of
    Left err -> Left $ formatError err
    Right ast -> Right ast

testParsing :: String -> IO ()
testParsing = parseTest pProg


spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty blockCmt where
    blockCmt = L.skipBlockCommentNested "(*" "*)"

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

decimal :: Integral a => Parser a
decimal = lexeme L.decimal

integer :: Integral a => Parser a
integer = L.signed spaceConsumer decimal

double :: Parser Double
double = L.signed spaceConsumer L.float

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

reservedWords = ["let", "in", "fun", "type"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_' <|> char '\'')
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

withPosition :: Parser (SourcePos -> a) -> Parser a
withPosition p = do
    pos <- getSourcePos
    f <- p
    return (f pos)

pDeclaration :: CtxParser LetDeclaration
pDeclaration = pDLet

pDLet :: CtxParser LetDeclaration
pDLet = withPosition $ do
    reserved "let"
    s <- identifier
    symbol "="
    e <- pExpr
    return (DLet s e)

pExpr :: CtxParser Expr
pExpr = pEApp

pExprSimpl :: CtxParser Expr
pExprSimpl = pELit <|> pEVar <|> pELet <|> pELambda <|> parens pExpr

pELit :: CtxParser Expr
pELit = withPosition (ELit <$> pLit)

pEVar :: CtxParser Expr
pEVar = withPosition (EVar <$> identifier)

pELet :: CtxParser Expr
pELet = withPosition $ do
    d <- pDLet
    reserved "in"
    e <- pExpr
    return (ELet d e)

many2 p = do
    a1 <- p
    a2 <- p
    many p >>= \l -> return $ a1 : a2 : l

pEApp :: CtxParser Expr
pEApp = try (foldl1 (\l r -> EApp l r (context l)) <$> (many2 pExprSimpl)) <|> pExprSimpl

pELambda :: CtxParser Expr
pELambda = withPosition $ do
    reserved "fun"
    (x, t) <- parens $ do
        x <- identifier
        symbol ":"
        t <- pBType
        return (x, t)
    symbol "->"
    e <- pExpr
    return (ELambda x t e)

pLit :: CtxParser Literal
pLit = withPosition (try pLInt <|> pLDouble <|> pLChar <|> pLString <|> pLUnit <|> pLBool)

pBType :: CtxParser BType
pBType = makeExprParser typeTerm typeOpers where
    typeTerm = parens pBType 
        <|> withPosition (TUnit <$ symbol "unit")
        <|> withPosition (TInt  <$ symbol "int")
        <|> withPosition (TStr  <$ symbol "string")
    typeOpers = [[InfixR (symbol "->" >> funPars)]]
    funPars = withPosition $ return $ \p x y -> TFun x y p

pLInt = LInt <$> integer
pLDouble = LDouble <$> double
pLChar = between (char '\'') (char '\'') (LChar <$> L.charLiteral)
pLString = LString <$> (char '\"' >> manyTill L.charLiteral (char '\"')) -- TODO add support for escapes
pLUnit = symbol "()" >> return LUnit
pLBool = LBool <$> try ((reserved "true" >> return True) <|> (reserved "false" >> return False))

pProg :: CtxParser Expr
pProg = between spaceConsumer eof pExpr
