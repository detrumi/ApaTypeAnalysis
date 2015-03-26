module Lexer where

import Text.Parsec.Prim
import Control.Applicative hiding ((<|>), many)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (letter, char)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

langDef :: P.LanguageDef u
langDef = haskellDef
    { P.identStart = letter <|> char '_'
    }

lexer :: P.TokenParser a
lexer = P.makeTokenParser langDef

opLetter :: String
opLetter = ":!#$%&*+./<=>?@\\^|-~"

parens, brackets, braces :: Parser a -> Parser a
parens   = P.parens lexer
brackets = P.brackets lexer
braces   = P.braces lexer

commaSep, commaSep1, semiSep1 :: Parser a -> Parser [a]
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer
semiSep1 = P.semiSep1 lexer

reserved, reservedOp :: String -> Parser ()
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

int :: Parser Int
int = fromInteger <$> P.natural lexer

identifier, operator, semi :: Parser String
identifier = P.identifier lexer
operator = P.operator lexer
semi = P.semi lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

identWith :: (String -> Bool) -> String -> Parser String
identWith p message = try $ do
    ident <- identifier
    if p ident then return ident else fail message