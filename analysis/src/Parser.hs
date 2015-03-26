module Parser where

import Expr
import Lexer

import Control.Applicative hiding ((<|>), many, Const)
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Pos (updatePosString)

type TokenParser a = Parsec [Token] () a

parens, brackets, braces :: TokenParser a -> TokenParser a
parens   = between (sStr "(") (sStr ")")
brackets = between (sStr "[") (sStr "]")
braces   = between (sStr "{") (sStr "}")

pStatements :: TokenParser [Statement]
pStatements = (pOperatorDecl <|> pData <|> SBind <$> pDef) `sepEndBy1` sStr ";" <* eof

pDef :: TokenParser Bind
pDef = Bind <$> (pLowerId <|> parens name) <*> many (pVar <|> try pConstrSimple <|> pConst) <* sStr "=" <*> pExpr
    where name = pLowerId <|> pOperator <|> parens name

pData :: TokenParser Statement
pData = SData <$ sStr "data" <*> pUpperId <*> many pLowerId
    <* sStr "=" <*> pDataConstr `sepBy1` sStr "|"

pOperatorDecl :: TokenParser Statement
pOperatorDecl = go <$ sStr "infixl" <*> sInt <*> pOperator `sepBy1` sStr ","
    where go p ops = SOperator $ map (\op -> (op, fromIntVal $ fromConst p)) ops

pExpr :: TokenParser Expr
pExpr = go <$> pExprSimple <*> many ((,) <$> pOperator <*> pExprSimple)
    where go e es = let (ops,es') = unzip es
                    in  case es of [] -> e
                                   _  -> Infix (e:es') ops

pOperator :: TokenParser String
pOperator = fromTerminal <$> satisfyT isOp
    where isOp (Terminal t) | all (`elem` symbols) t && (t `notElem` terminals) = True
          isOp _            = False

pExprSimple :: TokenParser Expr
pExprSimple = pLet
    <|> pIf
    <|> pCase
    <|> try (App <$> (parens pLambda <|> pVar) <*> some pExprSimple)
    <|> pLambda
    <|> try pConstr
    <|> pConst
    <|> pVar
    <|> parens pExpr

pConst :: TokenParser Expr
pConst = Const . fromConst <$> sConst
     <|> Const Unit <$ sStr "()"
     <|> Const . List <$> brackets (pExpr `sepBy` sStr ",")

pVar :: TokenParser Expr
pVar = Var <$> pLowerId

pLowerId :: TokenParser String
pLowerId = fromLowerId <$> sLowerId

pUpperId :: TokenParser TypeVar
pUpperId = fromUpperId <$> sUpperId

pLet :: TokenParser Expr
pLet = Let <$ sStr "let"
    <*> pDef `sepBy1` sStr ";"
    <* sStr "in" <*> pExpr

pIf :: TokenParser Expr
pIf = If <$ sStr "if" <*> pExpr <* sStr "then" <*> pExpr <* sStr "else" <*> pExpr

pCase :: TokenParser Expr
pCase = Case <$ sStr "case" <*> pExpr <* sStr "of" <*> braces (alt `sepEndBy1` sStr ";") 
    where alt = (,) <$> pExpr <* sStr "->" <*> pExpr

pLambda :: TokenParser Expr
pLambda = Lam <$ sStr "\\" <*> some pLowerId <* sStr "->" <*> pExpr

pConstr :: TokenParser Expr
pConstr = Con <$> pUpperId <*> many (pConst <|> pVar <|> pConstrSimple)

pConstrSimple :: TokenParser Expr
pConstrSimple = (\u -> Con u []) <$> pUpperId
            <|> parens pConstr

pDataConstr :: TokenParser DataCon
pDataConstr = DataCon <$> pUpperId <*> many pType

pType :: TokenParser Type
pType = TUnit <$ sStr "()"
    <|> TInt <$ sStr "Int"
    <|> TVar <$> pLowerId
    <|> TCon <$> pUpperId <*> many pType
    <|> parens pType

-- Token satisfiers --
sConst :: TokenParser Token
sConst = satisfyT isConst
  where isConst (ConstTok _) = True
        isConst _            = False

sLowerId :: TokenParser Token
sLowerId = satisfyT isLowerId
       where isLowerId (LowerId _) = True
             isLowerId _           = False

sUpperId :: TokenParser Token
sUpperId = satisfyT isUpperId
    where isUpperId (UpperId _) = True
          isUpperId _           = False

sTerminal :: TokenParser Token
sTerminal = satisfyT isTerminal
    where isTerminal (Terminal _) = True
          isTerminal _            = False

sStr :: String -> TokenParser Token
sStr t = satisfyT isStr
    where isStr (Terminal t') | t == t' = True
          isStr _                       = False

sInt :: TokenParser Token
sInt = satisfyT isInt
    where isInt (ConstTok (IntVal _)) = True
          isInt _                     = False

satisfyT :: (Show t, Stream s m t) => (t -> Bool) -> ParsecT s u m t
satisfyT f = tokenPrim (\t -> show [t])
                       (\pos t _ts -> updatePosString pos (show t))
                       (\t -> if f t then Just t else Nothing)