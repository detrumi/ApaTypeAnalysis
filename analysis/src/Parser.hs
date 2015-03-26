module Parser where

import Expr
import Lexer

import Control.Applicative hiding ((<|>), many, Const, optional)
import Text.Parsec.Prim
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Data.Char (isUpper, isLower)

pStatements :: Parser [Statement]
pStatements = sepEndBy1 (pOperatorDecl <|> pData <|> SBind <$> pDef) semi <* eof

pDef :: Parser Bind
pDef = Bind <$> (pLowerId <|> parens name) <*> many (pVar <|> try pConstrSimple <|> pConst) <* reservedOp "=" <*> pExpr
    where name = pLowerId <|> operator <|> parens name

pData :: Parser Statement
pData = SData <$ reserved "data" <*> pUpperId <*> many pLowerId
    <* reserved "=" <*> pDataConstr `sepBy1` reserved "|"

pOperatorDecl :: Parser Statement
pOperatorDecl = go <$ reserved "infixl" <*> int <*> commaSep1 operator
    where go p ops = SOperator $ map (\op -> (op, p)) ops

pExpr :: Parser Expr
pExpr = go <$> pExprSimple <*> many ((,) <$> operator <*> pExprSimple)
    where go e es = let (ops,es') = unzip es
                    in  case es of [] -> e
                                   _  -> Infix (e:es') ops

pExprSimple :: Parser Expr
pExprSimple = pLet
    <|> pIf
    <|> pCase
    <|> try (App <$> (parens pLambda <|> pVar) <*> some pExprSimple)
    <|> pLambda
    <|> try pConstr
    <|> pConst
    <|> pVar
    <|> parens pExpr

pConst :: Parser Expr
pConst = Const . IntVal <$> int
     <|> Const Unit <$ pUnit
     <|> Const . List <$> brackets (commaSep pExpr)

pVar :: Parser Expr
pVar = Var <$> pLowerId

pLowerId :: Parser String
pLowerId = identWith (\(h:_) -> isLower h || h == '_') "lowerid"

pUpperId :: Parser TypeVar
pUpperId = identWith (isUpper . head) "upperid"

pLet :: Parser Expr
pLet = Let <$ reserved "let"
    <*> semiSep1 pDef
    <* reserved "in" <*> pExpr

pIf :: Parser Expr
pIf = If <$ reserved "if" <*> pExpr <* reserved "then" <*> pExpr <* reserved "else" <*> pExpr

pCase :: Parser Expr
pCase = Case <$ reserved "case" <*> pExpr <* reserved "of" <*> braces (semiSep1 alt)
    where alt = (,) <$> pExpr <* reservedOp "->" <*> pExpr

pLambda :: Parser Expr
pLambda = Lam <$ reservedOp "\\" <*> some pLowerId <* reservedOp "->" <*> pExpr

pConstr :: Parser Expr
pConstr = Con <$> pUpperId <*> many (pConst <|> pVar <|> pConstrSimple)

pConstrSimple :: Parser Expr
pConstrSimple = (\u -> Con u []) <$> pUpperId
            <|> parens pConstr

pDataConstr :: Parser DataCon
pDataConstr = DataCon <$> pUpperId <*> many pType

pType :: Parser Type
pType = TUnit <$ pUnit
    <|> TInt <$ identWith (== "Int") "Int"
    <|> TVar <$> pLowerId
    <|> TCon <$> pUpperId <*> many pType
    <|> parens pType

pUnit :: Parser ()
pUnit = try (const () <$> symbol "()")
