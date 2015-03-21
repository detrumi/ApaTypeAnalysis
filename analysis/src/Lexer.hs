module Lexer where

import Expr

import Text.ParserCombinators.Parsec
import Control.Applicative hiding (Const, many, (<|>))
import Data.Char (isAlphaNum, isLower, isUpper)
import Control.Monad (liftM)


data Token = ConstTok { fromConst    :: Value }
           | LowerId  { fromLowerId  :: String }
           | UpperId  { fromUpperId  :: String }
           | Terminal { fromTerminal :: String }
           deriving (Eq)

instance Show Token where
    show (ConstTok v) = show v
    show (LowerId i) = i
    show (UpperId i) = i
    show (Terminal t) = t

keywords :: [String]
keywords =
    [ "let", "in"
    , "if", "then", "else"
    , "data"
    , "case", "of"
    , "infixl"]

terminals :: [String]
terminals =
    [ "()"
    , "->", ";"
    , "(", ")", "{", "}", "[", ",", "]"]

symbols :: [Char]
symbols = "!#$%&*+./<=>?@\\^|-~:"

lexSymbol :: Parser Token
lexSymbol = try $ Terminal <$> many1 (satisfy (`elem` symbols))

lexKeyword :: Parser Token
lexKeyword = do terminal <- choice $ map (try . (Terminal <$>) . string) keywords
                notFollowedBy alphaNum'
                return terminal

lexTerminal :: Parser Token
lexTerminal = choice $ map (try . (Terminal <$>) . string) terminals

lexSpaces :: Parser ()
lexSpaces = skipMany (space <|> const ' ' <$> try lexComment)

lexComment :: Parser String
lexComment = string "--" *> manyTill anyChar (newline <|> const ' ' <$> eof)
    <|> string "{-" *> manyTill anyChar (string "-}")

lexInt :: Parser Token
lexInt = ConstTok . IntVal . read <$> many1 digit 

lexLowerId :: Parser Token
lexLowerId = liftM LowerId . (:) <$> satisfy (withSpecials isLower) <*> many alphaNum'

lexUpperId :: Parser Token
lexUpperId = liftM UpperId . (:) <$> satisfy isUpper <*> many alphaNum'

lexicalScanner :: Parser [Token]
lexicalScanner = lexSpaces *> many (lexToken <* lexSpaces) <* eof

lexToken :: Parser Token
lexToken = choice $ map try
            [ lexInt
            , lexKeyword
            , lexTerminal
            , lexSymbol
            , lexLowerId
            , lexUpperId
            ]

withSpecials :: (Char -> Bool) -> Char -> Bool
withSpecials f c | f c       = True
                 | c == '_'  = True
                 | c == '\'' = True
                 | otherwise = False

alphaNum' :: Parser Char
alphaNum' = satisfy (withSpecials isAlphaNum)