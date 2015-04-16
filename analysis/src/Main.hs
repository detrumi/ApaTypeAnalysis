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
data Substitution = TypeVar :=>: Type -- substitute the first with the second

unification :: Type -> Type -> [Substitution]
unification TInt TInt = []
unification (TFunc a b) (TFunc c d) = unification a c ++ unification b d

solve :: [Constraint] -> [Substitution]
solve (CEmpty:cs)       = []
solve ((c1 :=: c2):cs)  = [] -- solve (s:cs) ++ s where s = unification c1 c2
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

		doParse :: String -> Either ParseError [Statement]
		doParse s = runP pStatements () "Main" s
