module Typechecker where

import Expr
import Parser

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Writer

data Constraint
	= Type :=: Type

type Env = M.Map Var Type

typecheck :: [Statement] -> [([Constraint], Type)]
typecheck ss = undefined

typecheck1 :: Env -> Statement -> ([Constraint], Type)
typecheck1 m (SBind (Bind v e)) = undefined -- M.insert ...
typecheck1 m _ = undefined

typecheck' :: Env -> Expr -> ([Constraint], Type)
typecheck' m expr = case expr of
	Const v -> ([], typecheckV v)
	Var v -> undefined

typecheckV :: Value -> Type
typecheckV (IntVal i) = TInt

