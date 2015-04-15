module Main where

import Data.Graph.Inductive

import Expr
import Parser
import Typechecker

import Text.Parsec.Prim (runP)
import Text.Parsec.Error (ParseError)
import System.Environment (getArgs)

-- TODO: verplaatst ergens anders naar toe
data Substitution = Type :=>: Type -- substitute the first with the second

unification :: Type -> Type -> [Substitution]
unification TInt TInt = []
unification (TFunc a b) (TFunc c d) = unification a c ++ unification b d

solve :: [Constraint] -> [Substitution]
solve cs = concatMap solve' cs where
	solve' CEmpty       = []
	solve' (c1 :=: c2)  = [] -- solve' s ++ s where s = unification c1 c2

main :: IO ()
main = do
	filename : _ <- getArgs
	input <- readFile filename
	run input
	where
		run :: String -> IO ()
		run input = putStrLn $ show $ unright $ doParse input

		unright (Right a) = a
		unright (Left _)  = []

		doParse :: String -> Either ParseError [Statement]
		doParse s = runP pStatements () "Main" s
