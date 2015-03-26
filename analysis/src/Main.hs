module Main where

import Expr
import Lexer
import Parser

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

doLex :: String -> Either ParseError [Token]
doLex s = runP lexicalScanner () "Main" s

doParse :: String -> Either ParseError [Statement]
doParse s = do
    s' <- doLex s
    runP pStatements () "Main" s'