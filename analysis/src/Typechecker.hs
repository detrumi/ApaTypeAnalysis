module Typechecker where

import Expr
import Parser

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Writer

data Constraint
	= Type :=: Type

type Env = M.Map Var Type
type CWriter = Writer [Constraint]
type EnvState = StateT Env CWriter

typecheck :: [Statement] -> CWriter [Type]
typecheck ss = evalStateT (mapM typecheck1 ss) M.empty

typecheck1 :: Statement -> EnvState Type
typecheck1 (SBind (Bind v e)) = typecheck' e -- M.insert ...
typecheck1 _ = undefined

typecheck' :: Expr -> EnvState Type
typecheck' expr = case expr of
	Const v -> return $ typecheckV v

typecheckV :: Value -> Type
typecheckV (IntVal i) = TInt
