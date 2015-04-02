module Main where

import Expr
import Parser
import Typechecker

import Text.Parsec.Prim (runP)
import Text.Parsec.Error (ParseError)
import System.Environment (getArgs)

main :: IO ()
main = do
	filename : _ <- getArgs
	input <- readFile filename
	run input

run :: String -> IO ()
run input = putStrLn $ show $ doParse input

doParse :: String -> Either ParseError [Statement]
doParse s = runP pStatements () "Main" s