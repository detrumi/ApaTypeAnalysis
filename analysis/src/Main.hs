module Main where

import Data.Graph.Inductive

import Expr
import Parser

import Text.Parsec.Prim (runP)
import Text.Parsec.Error (ParseError)
import System.Environment (getArgs)

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
