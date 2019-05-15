module Parser where

import Control.Monad
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec String String

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