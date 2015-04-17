module Typechecker where

import Expr
import Parser

import Data.Char (chr, ord)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Monad.State

-- TODO: nog maar even kijken of we beide intance constraints nodig hebben
-- deze zijn gelijk aan die in Generalizing HM
data Constraint =
	  CEmpty
	| Type :=: Type
	| Type :-<: Type    -- Explicit instance constraint
	| Type :<: Type     -- Implicit instance constraint
    deriving (Eq, Show)

type Env = M.Map Var Type
type Counter = State String


typecheck :: [Statement] -> [([Constraint], Type)]
typecheck ss = evalState (sequence (map (typecheck1 M.empty) ss)) "A"

typecheck1 :: Env -> Statement -> Counter ([Constraint], Type)
typecheck1 m (SBind (Bind v e)) = typecheck' m e
typecheck1 m _ = return ([], TInt)

typecheck' :: Env -> Expr -> Counter ([Constraint], Type)
typecheck' m expr = case expr of
    Const v -> return ([], typecheckV v)
    Var v -> do
        let t = fromMaybe (error $ "Unknown variable: " ++ v) (M.lookup v m)
        return ([], t)
    Let bs e -> do
        -- TODO what to do with bs here?
        e' <- typecheck' m e
        return e'
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
    Case e alts -> do
        (c',e') <- typecheck' m e
        cases <- mapM (typecheck' m) (map fst alts)
        alts' <- mapM (typecheck' m) (map snd alts)
        let cs = c' ++ concatMap fst cases ++ concatMap fst alts' ++ map (e' :=:) (map snd cases) ++ (\(t:ts) -> map (t :=:) ts) (map snd alts')
        return (cs, snd (head alts'))
    If i t e -> do
        (ci,ti) <- typecheck' m i
        (ct,tt) <- typecheck' m t
        (ce,te) <- typecheck' m e
        return (ci ++ ct ++ ce ++ [tt :=: te], tt)
    Infix [e1,e2] [op] -> do
        (c1,t1) <- typecheck' m e1
        (c2,t2) <- typecheck' m e2
        return ([t1 :=: TInt, t2 :=: TInt] ++ c1 ++ c2, TInt)
    Con v es -> do
        es' <- mapM (typecheck' m) es
        let t = fromMaybe (error $ "Unknown constructor: " ++ v) (M.lookup v m)
        return (concatMap fst es', t)


typecheckV :: Value -> Type
typecheckV (IntVal i) = TInt

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
