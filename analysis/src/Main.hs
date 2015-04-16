module Main where

import Data.Graph.Inductive

import Expr
import Parser
import Typechecker

import Text.Parsec.Prim (runP)
import Text.Parsec.Error (ParseError)
import System.Environment (getArgs)
import Data.Either (rights)

-- TODO: verplaatst ergens anders naar toe
data Substitution = Var :=>: Type -- substitute the first with the second

unification :: Type -> Type -> [Substitution]
unification TInt TInt = []
unification (TFunc a b) (TFunc c d) = unification a c ++ unification b d

-- Apply substitution to constraints
substitute :: Substitution -> [Constraint] -> [Constraint]
substitute sub cs = map (f sub) cs where
	f (v :=>: t) (c :=: d) = repl v t c :=: repl v t d
	f (v :=>: t) (c :-<: d) = repl v t c :-<: repl v t d
	f (v :=>: t) (c :<: d) = repl v t c :<: repl v t d

	repl :: Var -> Type -> Type -> Type
	repl v t (TVar x)   | v == x    = t
						| otherwise = TVar x -- Just create a new TVar, to replace the old one
	repl v t TInt           = TInt
	repl v t (TCon tv ts)   = TCon tv (map (repl v t) ts)
	repl v t (TFunc f1 f2)  = TFunc (repl v t f1) (repl v t f2)

solve :: [Constraint] -> [Substitution]
solve (CEmpty:cs)       = []
solve ((c1 :=: c2):cs)  = [] -- solve (substitute s cs) ++ s where s = unification c1 c2
--solve ((c1 :<: c2):cs)
--solve ((c1 :-<: c2):cs)

main :: IO ()
main = do
	filename : _ <- getArgs
	input <- readFile filename
	run input
	where
		run :: String -> IO ()
		run input = putStrLn $ show $ typecheck $ either (error . show) id $ doParse input

		unright (Right a) = a
		unright (Left _)  = []

		doParse :: String -> Either ParseError [Statement]
		doParse s = runP pStatements () "Main" s
