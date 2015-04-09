module Main where

import Data.Graph.Inductive

import Expr
import Parser

import Text.Parsec.Prim (runP)
import Text.Parsec.Error (ParseError)
import System.Environment (getArgs)

-- Main Type, heeft dit nog iets te maken met Type uit Expr.hs?
data MType = MInt | MBool | MFun MType MType deriving Show


-- types en shit
-- TODO: we moeten hier nog een graaf van maken
-- TODO: nog niet alles klopt
ty :: [Statement] -> MType
ty stmts = undefined where
	tyS :: Statement -> MType
	tyS (SBind (Bind v e))  = tyE e
	tyS (SOperator xs)      = undefined -- TODO
	tyS (SData _ _ _)             = undefined

	tyE :: Expr -> MType
	tyE (Const v) = tyV v where
	tyE (Var v)     = MInt -- TODO: waarom is het type van een Var een string?
	tyE (Let bs e)  = tyE e
	tyE (Lam vs e)  = MFun MInt (tyE e) -- TODO: lijst van vars in Lam? Moeten we niet een Lam met maar 1 var maken, en alles hier naar toe omschrijven? TODO: ook nog het type van vs krijgen
	tyE (App f (e:_)) = apply f e where -- TODO: App een lijst van expressies? Een functie pas je toch maar toe op 2 argumenten? Netzoals bij Lam
		apply f e = apply' (tyE f) where
			apply' (MFun t1 t2) = t2
			apply' (_)          = error "First argument to App is not a function type!"
	tyE (If e1 e2 e3)   = tyE e2
	tyE (Case e es)     = tyE $ snd $ head es
	tyE (Con c es)      = undefined -- TODO
	tyE (Infix es ops)  = tyE (head es) -- TODO: weer die lijsten... TODO: moet nog worden ingevuld

	tyV :: Value -> MType
	tyV (IntVal x)    = MInt
	tyV (List (e:_))  = tyE e -- Just return type of head



-- DIT HEBBEN WE NIET MEER NODIG
-- Function to create the CFG from the list of statements
--cfg :: [Statement] -> Gr String ()
{-cfg ss = map (\t -> lnodeS t 0) ss where
	lnodeS :: Statement -> Int -> [LNode String]
	lnodeS (SBind (Bind v e))   i = (i, show v) : lnodeE e i
	lnodeS (SOperator xs)       i = [(10, "")] -- TODO
	lnodeS (SData t vs ds)      i = [(20, "")]

	lnodeE :: Expr -> Int -> [LNode String]
	lnodeE (Const v)    i   = [(i, show v)]
	lnodeE (Var v)      i   = [(i, show v)]
	lnodeE (Let bs e)   i   = (i, concatMap show bs) : lnodeE e i
	lnodeE (Lam vs e)   i   = (i, concatMap show vs) : lnodeE e i
	lnodeE (App e es)   i   = lnodeE e i ++ (concatMap (\t -> lnodeE t i) es)
	lnodeE (If e1 e2 e3) i  = lnodeE e1 i ++ lnodeE e2 i ++ lnodeE e3 i
	lnodeE (Case e es)  i   = [(-10, "")] -- TODO
	lnodeE (Con t es)   i   = (i, show t) : (concatMap (\t -> lnodeE t i) es)
	lnodeE (Infix es ops) i = concatMap (\t -> lnodeE t i) es -- TODO: iets met de operators-}

main :: IO ()
main = do
	filename : _ <- getArgs
	input <- readFile filename
	run input
	where
		run :: String -> IO ()
		run input = putStrLn $ show $ ty $ unright $ doParse input

		unright (Right a) = a
		unright (Left _)  = []

		doParse :: String -> Either ParseError [Statement]
		doParse s = runP pStatements () "Main" s
