module Main where

import Data.Graph.Inductive

import Expr
import Parser

import Text.Parsec.Prim (runP)
import Text.Parsec.Error (ParseError)
import System.Environment (getArgs)


-- TODO: moet nog veel aan gebeuren, oa de i van de nodes
-- Function to create the CFG from the list of statements
--cfg :: [Statement] -> Gr String ()
cfg ss = map (\t -> lnodeS t 0) ss where
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
	lnodeE (Infix es ops) i = concatMap (\t -> lnodeE t i) es -- TODO: iets met de operators

main :: IO ()
main = do
	filename : _ <- getArgs
	input <- readFile filename
	run input
	where
		run :: String -> IO ()
		run input = putStrLn $ show $ cfg $ unright $ doParse input

		unright (Right a) = a
		unright (Left _)  = []

		doParse :: String -> Either ParseError [Statement]
		doParse s = runP pStatements () "Main" s
