module Typechecker where

import Expr
import Parser

import Data.Char (chr, ord)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Writer

data Constraint
    = Type :=: Type

type Env = M.Map Var Type
type Counter = State String


typecheck :: [Statement] -> Counter [([Constraint], Type)]
typecheck ss = undefined

typecheck1 :: Env -> Statement -> Counter ([Constraint], Type)
typecheck1 m (SBind (Bind v e)) = undefined -- M.insert ...
typecheck1 m _ = undefined

typecheck' :: Env -> Expr -> Counter ([Constraint], Type)
typecheck' m expr = case expr of
    Const v -> return ([], typecheckV v)
    Var v -> do
        let t = fromMaybe (error $ "Unknown variable: " ++ v) (M.lookup v m)
        return ([], instantiate t)
    Let bs e -> undefined
    Lam x e -> do
        b <- fresh
        let m' = M.insert x b m
        (cs, t) <- typecheck' m' e
        return (cs, b `TFunc` t)
    App e1 e2 -> do
        (c1, t1) <- typecheck' m e1
        (c2, t2) <- typecheck' m e2
        x <- fresh
        let c = c1 ++ c2 ++ [t1 :=: (t2 `TFunc` x)]
        return (c, x)

typecheckV :: Value -> Type
typecheckV (IntVal i) = TInt

instantiate :: Type -> Type
instantiate = undefined


-- Get a fresh variable, and increase the state
fresh :: Counter Type
fresh = do
    s <- get
    modify nextFresh
    return (TVar s)

nextFresh :: TypeVar -> TypeVar
nextFresh "Z" = "AA"
nextFresh u = case last u of
    'Z' -> (nextFresh . init) u ++ "A"
    c   -> init u ++ [chr (ord c + 1)]
