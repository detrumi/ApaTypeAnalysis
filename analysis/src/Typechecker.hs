module Typechecker where

import Expr
import Parser

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Writer

-- TODO: nog maar even kijken of we beide intance constraints nodig hebben
-- deze zijn gelijk aan die in Generalizing HM
data Constraint =
	| Type :=: Type
	| Type :-<: Type    -- Explicit instance constraint
	| Type :<: Type     -- Implicit instance constraint

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

